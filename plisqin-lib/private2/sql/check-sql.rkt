#lang racket

; This is just a helper for rackunit tests
(provide check-sql)

(require "../_types.rkt"
         "../_dialect.rkt"
         rackunit
         (only-in "../_core.rkt" to-sql))

(define-syntax (get-dialect stx)
  (syntax-case stx ()
    [(_ #:ms) #'(mssql)]
    [(_ #:pg) #'(postgres)]
    [(_ #:lite) #'(sqlite)]))

(define-syntax (check-single stx)
  (syntax-case stx ()
    [(_ frag kw expected)
     #`(if expected
           (let* ([dialect (get-dialect kw)]
                  [actual (parameterize ([current-dialect dialect])
                            (to-sql frag))])
             #,(syntax/loc stx
                 (check-equal? (string-normalize-spaces actual)
                               (string-normalize-spaces expected)
                               (format "with dialect ~a" dialect))))
           (void))]))

(define-syntax (check-sql stx)
  (syntax-case stx ()
    [(_ frag args ...)
     #`(begin
         (let ([proc (λ (f #:pg [pg-expected #f]
                           #:ms [ms-expected #f]
                           #:lite [lite-expected #f]
                           #:all [all-expected #f])
                       #,(syntax/loc stx
                           (check-single f #:pg (or pg-expected all-expected)))
                       #,(syntax/loc stx
                           (check-single f #:ms (or ms-expected all-expected)))
                       #,(syntax/loc stx
                           (check-single f #:lite (or lite-expected all-expected))))])
           #,(syntax/loc stx
               (proc frag args ...))))]))
