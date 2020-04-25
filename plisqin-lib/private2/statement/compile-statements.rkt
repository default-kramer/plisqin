#lang racket

(provide compile-statements
         statement-sql)

(require racket/struct
         (prefix-in db: db)
         (for-syntax "define-statement.rkt"
                     "param.rkt"
                     (only-in racket/function identity)
                     (only-in "../_core.rkt" to-sql)
                     (only-in "../_dialect.rkt" current-dialect)))


(define-syntax (compile-statements stx)
  ; TODO before making public:
  ; 1) use syntax-parse and probably add keywords like #:dialect
  ; 2) check error messages when given a bad module or dialect
  ; 3) check error messages when to-sql fails -- it should at least tell you
  ;    which statement failed
  (syntax-case stx ()
    [(_ :module :dialect)
     (let* ([dialect (eval-syntax #':dialect)]
            [modpath `(submod ,(syntax-e #':module) plisqin-reserved:statements-to-compile)]
            [statement-ids (dynamic-require modpath 'plisqin-reserved:statement-ids)])
       (quasisyntax/loc stx
         (begin
           #,@(for/list ([id statement-ids])
                (let*-values
                    ([(us) ; us = uncompiled-statement
                      (dynamic-require modpath id)]
                     [(params)
                      (uncompiled-statement-params us)]
                     [(token)
                      (uncompiled-statement-result us)]
                     [(sql)
                      (parameterize ([current-dialect dialect])
                        (to-sql token))]
                     [(sql rearrange)
                      (if (need-to-rewrite? dialect)
                          (rewrite-params sql params)
                          (values sql identity))]
                     [(param-ids)
                      (generate-temporaries (map (λ(x) 'param) params))]
                     [(arranged-param-ids)
                      (rearrange param-ids)])
                  (with-syntax ([id (datum->syntax stx id stx)]
                                [bind-param-values (datum->syntax #f id stx)]
                                [sql-literal sql]
                                [(param-id ...) param-ids]
                                [(arranged-param-id ...) arranged-param-ids])
                    #'(begin
                        (define sql sql-literal)
                        (define (bind-param-values param-id ...)
                          (bound-statement sql (list arranged-param-id ...)))
                        (define id
                          (unbound-statement bind-param-values 'id sql))
                        (provide id))))))))]))

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
