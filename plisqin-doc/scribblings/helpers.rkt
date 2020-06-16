#lang racket

(provide my-eval reset-eval! make-eval
         show-sql check-sql check-sql2
         to-table
         load-checkpoint! repl repl-query
         def-green-ids code:strike
         )

(require scribble/manual
         (only-in scribble/racket make-element-id-transformer)
         racket/stxparam
         racket/require
         (subtract-in scribble/eval scribble/example)
         scribble/example
         plisqin-lib
         plisqin-examples/adventure-works
         (prefix-in db: db)
         racket/exn
         rackunit
         "to-table.rkt")
(require (for-syntax syntax/strip-context))

(define (make-eval)
  (let ([eval (make-base-eval)])
    (interaction-eval #:eval eval
                      (require racket/match
                               racket/contract)
                      (require (only-in racket string-join))
                      (require (prefix-in aw: plisqin-examples/adventure-works)
                               (prefix-in db: db)
                               ; figure out what, if any, prefix we want:
                               plisqin-examples/adventure-works/schema
                               plisqin)
                      (define-syntax-rule (code:hilite x) x)
                      )
    eval))

(define my-eval (make-eval))

(define (reset-eval!)
  (set! my-eval (make-eval)))
(reset-eval!)

(define (show-sql str)
  (verbatim str))

; use a macro here so that DrRacket takes you right to the failure
(define-syntax-rule (check-sql eval q str)
  (let ([eval-sql
         (with-handlers ([exn? (λ(ex) "")])
           (eval '(to-sql q)))])
    (check-equal?
     (string-normalize-spaces eval-sql)
     (string-normalize-spaces str))))

(define (check-sql2 actual expected)
  (check-equal?
   (string-normalize-spaces actual)
   (string-normalize-spaces expected)))

(define (load-checkpoint! checkpoint)
  (set! checkpoint (string-replace checkpoint ".rkt" ""))
  (define cp-path (string-append "plisqin-doc/scribblings/adventure-works-checkpoints/" checkpoint))
  (reset-eval!)
  (my-eval `(begin
              (require plisqin-examples/adventure-works
                       plisqin)
              (require ,(string->symbol cp-path))
              (current-dialect (sqlite)))))

; This should be a procedure that works on a datum that replaces
; for example 'PRODUCTID with 'ProductID
(define-syntax-parameter undo-green-ids
  (lambda anything #'identity))

(define-syntax (show-results stx)
  (syntax-case stx ()
    [(_ orig-datum)
     (quasisyntax/loc stx
       (let ([query-datum (#%app undo-green-ids orig-datum)])
         (show-results2 query-datum
                        (λ (ex)
                          #,(quasisyntax/loc stx
                              (error 'show-results "eval failed\n~a\n~a"
                                     query-datum (exn->string ex)))))))]))

(define (show-results2 query-datum error-proc)
  ; Use `my-eval` to get the sql from the query-datum.
  ; But we need to get the db:rows-result in our own evaluation context.
  ; This is OK, we can trust that (db:query adventure-works-conn sql)
  ; will work exactly the same in both evaluation contexts.
  (define sql
    (with-handlers ([exn? (λ(ex)
                            (begin
                              (displayln ex)
                              (error-proc ex)))])
      (my-eval `(parameterize ([current-dialect (sqlite)])
                  (to-sql ,query-datum)))))
  (define result
    (let* ([conn (connect-adventure-works)]
           [result (with-handlers ([exn? (λ(ex) (displayln sql) (raise ex))])
                     (db:query conn sql))])
      (db:disconnect conn)
      result))
  (to-table result sql))

(define-syntax-rule (repl form ...)
  (examples #:eval my-eval
            #:label #f
            form ...))

(define-syntax (repl-query stx)
  (syntax-case stx ()
    [(_ show-table-form)
     (let* ([query (syntax-case #'show-table-form ()
                     [(_ q) #'q])]
            [query-datum (syntax->datum query)])
       #`(nested
          (racketinput show-table-form)
          #,(quasisyntax/loc stx
              (show-results '#,query-datum))))]))

(define-syntax-rule (code:strike form)
  (elem #:style PStrike (racket form)))

; The idea is that the caller should pass in `repl-query` which will modify
; the binding so that it is aware of the green IDs and can reverse them before
; trying to eval the code.
(define-syntax (def-green-ids stx)
  (syntax-case stx ()
    [(_ repl-query-new [ID id] ...)
     (with-syntax ([ooo (quote-syntax ...)])
       #'(begin
           (define-syntax ID
             (make-element-id-transformer
              (lambda anything #'(elem #:style PGreen (racket id)))))
           ...
           (define (ugi datum)
             (case datum
               [(ID) 'id]
               ...
               [else (if (pair? datum)
                         (cons (ugi (car datum))
                               (ugi (cdr datum)))
                         datum)]))
           (define-syntax-rule (repl-query-new stuff ooo)
             (syntax-parameterize ([undo-green-ids (lambda anything #'ugi)])
               (repl-query stuff ooo)))))]))
