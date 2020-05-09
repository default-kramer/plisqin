#lang racket

(provide my-eval reset-eval! make-eval
         show-sql check-sql check-sql2
         to-table
         load-checkpoint! show-results repl repl-query
         )

(require scribble/manual
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
                      (require racket/match)
                      (require (only-in racket string-join))
                      (require (prefix-in aw: plisqin-examples/adventure-works)
                               (prefix-in db: db)
                               ; TODO figure out what, if any, prefix we want:
                               plisqin-examples/adventure-works/schema
                               plisqin))
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

(define-syntax (show-results stx)
  (syntax-case stx ()
    [(_ query-datum)
     (quasisyntax/loc stx
       (show-results2 query-datum
                      (λ (ex)
                        #,(quasisyntax/loc stx
                            (error 'show-results "eval failed\n~a\n~a"
                                   query-datum (exn->string ex))))))]))

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
