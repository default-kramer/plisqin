#lang racket

(provide make-param need-to-rewrite? rewrite-params)
; Be careful about providing anything more!
; Parameters should be completely opaque so that it is difficult (ideally
; impossible) to make decisions based on them.
; The decision will only be made during SQL-generation time with an unbound
; parameter, not by the DB with a bound parameter.

; TODO illegal require here, should be "../_token.rkt" or something like that
(require "../sql/token.rkt"
         "../_types.rkt"
         "../_dialect.rkt")


; Define an "indexed parameter" to mean a parameter whose binding position
; is based on a number, as opposed to where it occurs in the SQL string.
; This is nice for us because when we create parameters, we don't know where
; they will land in the generated SQL. They might even be repeated or unused.
;
; SQLite supports indexed parameters written ?42
; PostgreSQL supports indexed parameters written $42
; MSSQL via ODBC (and perhaps all ODBC?) does not support indexed parameters.
; So we have to do some rewriting work to support it.
(define (need-to-rewrite? dialect)
  ; TODO investigate. I suspect this is not a function of the dialect.
  ; I suspect it is a function of `using-odbc?`.
  ; Meaning if someone is using PostgreSQL via ODBC this will not work for them.
  (and (not (postgres? dialect))
       (not (sqlite? dialect))))

(define (make-param index #:id [id #f] #:type [type #f])
  (new param%
       [index index]
       [id id]
       [type (or type Scalar)]))

(define param%
  (class* token% ()
    (inherit-field type as-name nullability fallback)
    (init-field index id)
    (super-new)

    (define/override (change #:cast [type type]
                             #:as [as-name as-name]
                             #:null [nullability nullability]
                             #:fallback [fallback fallback])
      (new param%
           [index index]
           [id id]
           [type type]
           [as-name as-name]
           [nullability nullability]
           [fallback fallback]))

    ; sql-token<%>
    (define/override (token-kind)
      'scalar)
    (define/override (token-content)
      (list))
    (define/override (sql-token-reduce)
      (define cd (current-dialect))
      (cond
        [(need-to-rewrite? cd)
         ; In the (hopefully) unlikely event that this pattern occurs naturally
         ; in the SQL, bind-prepared-statement will raise an error that we
         ; provided the wrong number of parameter values.
         ; So while this isn't bulletproof, it's not too bad.
         (format "#<<{param:~a}>>#" index)]
        [(sqlite? cd)
         (format "?~a /*~a*/" index id)]
        [(postgres? cd)
         (format "$~a /*~a*/" index id)]
        [else
         (error "cannot render params in dialect:" cd)]))

    ; equal<%>
    (define/override (equal-content)
      ; Haven't given this too much thought:
      (list index id))
    ))

(module+ test
  (require rackunit
           "../_types.rkt")
  (define p (make-param 1 #:id 'foo))
  (define p:Number (>> p #:cast Number))
  (check-equal? p p:Number)
  (check-equal? (get-type p:Number)
                Number))


; This regex matches our special pattern, eg #<<{param:123}>>#
(define px #px"#<<\\{param:[\\d]+\\}>>#")

; Assume that we need to rewrite (we are using MSSQL via ODBC).
; We cannot use named parameters; they only work with SPs.
;  (source https://docs.microsoft.com/en-us/sql/odbc/reference/develop-app/binding-parameters-by-name-named-parameters?view=sql-server-ver15)
; Seemingly our only option is to rewrite the generated SQL and rearrange the
; parameter values before binding them.
; For example, let's say the generated SQL is
#;"select * from foo where x = <param2> and y = <param2> and z = <param1>"
; We rewrite this to
#;"select * from foo where x = ? and y = ? and z = ?"
; And produce a "rearrange" function such that
#;(equal? (rearrange '(1 2))
          '(2 2 1))
; The rearrange function will be used to put the param values in the correct
; order before calling bind-prepared-statement.
(define/contract (rewrite-params sql params)
  ; Returns two values: the rewritten SQL and the rearrange function.
  (-> string? (listof (is-a?/c param%)) any)
  (define matches (regexp-match* px sql))
  (set! sql (regexp-replace* px sql "?"))

  (define (get-index match-str)
    ; we know that match-str looks like "#<<{param:123}>>#"
    (string->number (car (regexp-match #px"\\d+" match-str))))
  (define indexes (map get-index matches))

  (define (rearranger lst)
    (define v (list->vector lst))
    (map (λ (i) (vector-ref v (sub1 i))) indexes))
  (values sql rearranger))

(module+ test
  (define $foo (make-param 1 #:id 'foo))
  (define $bar (make-param 2 #:id 'bar))

  (let-values ([(sql rearranger)
                (rewrite-params "here we go #<<{param:2}>># #<<{param:2}>># #<<{param:1}>>#"
                                (list $foo $bar))])
    (check-equal? sql
                  "here we go ? ? ?")
    (check-equal? (rearranger '(foo bar))
                  '(bar bar foo)))

  (let-values ([(sql rearranger)
                ; param:1 is never used
                (rewrite-params "#<<{param:2}>># more fake SQL"
                                (list $foo $bar))])
    (check-equal? sql
                  "? more fake SQL")
    (check-equal? (rearranger '(foo bar))
                  '(bar))))
