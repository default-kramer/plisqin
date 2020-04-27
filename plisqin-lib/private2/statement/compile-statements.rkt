#lang racket

(provide compile-statements
         statement-sql)

(require racket/struct
         (prefix-in db: db)
         "define-statement.rkt"
         "param.rkt"
         ; TODO fix name clash that makes this necessary:
         (prefix-in p: "param.rkt")
         (only-in "../_dialect.rkt" current-dialect)
         (only-in "../_core.rkt" to-sql)
         (for-syntax "define-statement.rkt"
                     racket/sequence
                     "param.rkt"
                     racket/syntax))


; TODO this needs an overhaul - the `default` here is just a flag as to whether
; or not the arg has a default value. If it does, then TODO points to it.
(define-for-syntax (format-arglist stx)
  ; Flatten a syntax object like
  #;([#:keyword [arg-id default TODO]]
     ...)
  ; into an argument list that `define` will accept.
  ; The trick is that each #:keyword and default might be #f to indicate its absence.
  (define (handle-arg stx)
    (syntax-case stx ()
      [[arg-id default TODO]
       (if (syntax-e #'default)
           (syntax/loc stx
             [arg-id TODO])
           #'arg-id)]))
  (syntax-case stx ()
    [()
     (list)]
    [([keyword arg] rest ...)
     (let ([arg* (handle-arg #'arg)]
           [more (format-arglist #'(rest ...))])
       (if (syntax-e #'keyword)
           (cons #'keyword (cons arg* more))
           (cons arg* more)))]))

(define-for-syntax (compile-one-new modpath raw-id dialect stx root-modpath)
  (let* ([us ; us = uncompiled-statement
          (dynamic-require modpath raw-id)]
         [params
          (uncompiled-statement-params us)]
         [param-ids
          (map param-id params)])
    ; TODO major cleanup needed!
    (with-syntax ([(param-id ...) param-ids]
                  [(index-seq ...)
                   (sequence->list (in-range 0 (length param-ids)))]
                  [(param-kw ...)
                   (map param-kw params)]
                  [(param-default ...)
                   (map param-default params)])
      (with-syntax ([(param-default-value ...)
                     (generate-temporaries #'(param-id ...))]
                    [id (datum->syntax stx raw-id stx)]
                    [param-list (generate-temporary "param-list")]
                    [token (generate-temporary "token")]
                    [bind-param-values (generate-temporary "bind-param-values")]
                    [orig-sql (generate-temporary "orig-sql")]
                    [sql (generate-temporary "sql")]
                    [rearrange (generate-temporary "rearrange")]
                    [dialect dialect]
                    [root-modpath root-modpath]
                    [us (generate-temporary "uncompiled-statement")])
        ; We could do things like to-sql and rewrite-params right now instead of in
        ; the code we are generating. In fact, I had it working that way but then
        ; got tripped up with handling the default values.
        ; Well, now that it is working it should be possible to refactor in that
        ; direction if I want to.
        ; But be careful -- some error conditions may be difficult to message well.
        ; WAIT - does doing to-sql at runtime allow the user to switch dialects
        ; based on a config setting? I think it does!
        #`(begin
            (require (rename-in (submod root-modpath plisqin-reserved:statements-to-compile)
                                [#,raw-id us]))
            (define param-list
              (uncompiled-statement-params us))
            (define param-default-value
              (p:param-default (list-ref param-list index-seq)))
            ...
            (define token
              (uncompiled-statement-result us))
            (define orig-sql
              (parameterize ([current-dialect dialect])
                (to-sql token)))
            (define-values (sql rearrange)
              (if (need-to-rewrite? dialect)
                  (rewrite-params orig-sql param-list)
                  (values orig-sql identity)))
            (define (bind-param-values
                     #,@(format-arglist #'([param-kw [param-id param-default param-default-value]]
                                           ...)))
              (bound-statement sql (rearrange (list param-id ...))))
            (define id
              (unbound-statement bind-param-values 'id sql))
            (provide id)
            )))))

(define-syntax (compile-statements stx)
  ; TODO before making public:
  ; 1) use syntax-parse and probably add keywords like #:dialect
  ; 2) check error messages when given a bad module or dialect
  ; 3) check error messages when to-sql fails -- it should at least tell you
  ;    which statement failed
  (syntax-case stx ()
    [(_ :module :dialect)
     (let* ([modpath `(submod ,(syntax-e #':module) plisqin-reserved:statements-to-compile)]
            [statement-ids (dynamic-require modpath 'plisqin-reserved:statement-ids)])
       (quasisyntax/loc stx
         (begin
           #,@(for/list ([id statement-ids])
                (compile-one-new modpath id #':dialect stx #':module)))))]))

; A procedure that accepts the parameter values to produce a statement
(struct unbound-statement (proc id sql)
  #:property prop:procedure 0
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (unbound-statement-printer me port #t))])

(define unbound-statement-printer
  (make-constructor-style-printer
   (lambda (us) 'unbound-statement)
   (lambda (us) (list (unbound-statement-id us)))))

(struct bound-statement (sql param-values) #:transparent
  #:property db:prop:statement
  (lambda (me conn)
    (define sql (bound-statement-sql me))
    (define param-values (bound-statement-param-values me))
    (define prepared (db:prepare conn sql))
    ; TODO saw this pattern here: https://github.com/rmculpepper/sql/blob/792895fae759c6ef60aff054c1f707bb4f15407a/private/syntax.rkt#L148
    ; Not sure if this is necessary, or just an optimization:
    (if (empty? param-values)
        prepared
        (db:bind-prepared-statement prepared param-values))))

(define/contract (statement-sql s)
  (-> (or/c unbound-statement? bound-statement?) string?)
  (cond
    [(unbound-statement? s)
     (unbound-statement-sql s)]
    [(bound-statement? s)
     (bound-statement-sql s)]))
