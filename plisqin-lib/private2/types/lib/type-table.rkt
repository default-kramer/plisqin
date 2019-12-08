#lang racket

(provide def-type-table)

(require "core.rkt"
         "guard.rkt"
         racket/stxparam)

(define-for-syntax (my-return-handler stx)
  (syntax-case stx ()
    [(_ retval Type)
     ; This should always succeed, unless we start returning non-typed<%> objects,
     ; or using return contracts that are not types.
     #'(assign-type! retval Type)]))

(define-syntax (guard2 stx)
  (syntax-case stx ()
    [(_ id (ignored clause ...))
     #'(syntax-parameterize ([return-handler my-return-handler])
         (guard id clause ...))]))

(define-syntax (def-type-table stx)
  (syntax-case stx ()
    [(_ table-id :table-id
        [(id ...) body]
        ...)
     #'(begin
         (define (table-id sym)
           (case sym
             [(id ...) #'body]
             ...
             [else #f]))
         (define-for-syntax (:table-id orig-id)
           (case (syntax-e orig-id)
             [(id ...)
              #`(guard2 #,orig-id body)]
             ...
             [else (error "Missing type for" orig-id)])))]))
