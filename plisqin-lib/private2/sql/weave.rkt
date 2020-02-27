#lang racket

(provide def-dispatcher define/weave)
(provide (for-syntax dispatcher))

(require "fragment.rkt")

; A "head" is a syntax object with a shape like (= a b).
; If used like (define <head> body) it would define a procedure.
; A "dispatcher" is a procedure that accepts a head and returns another
; syntax object to be used in expression position.
; We will have the following dispatchers:
; 1) The retval dispatcher should return an expression to be used as the
;    return value (or "retval") of the procedure.
; 2) The typecheck dispatcher should return an expression that validates the
;    argument types and determines the ReturnType to be assigned to the retval.
; 3) The nullcheck dispatcher should return an expression that determines
;    whether we know anything about the nullability of the retval.
; Separating these concerns will allow us to weave together different variants
; of typechecking and nullchecking to the basic procedure.

; A dispatcher is implemented as a syntax transformer that we can recognize:
(begin-for-syntax
  (struct dispatcher (proc) #:property prop:procedure 0))

(define-for-syntax (get-proc-id head)
  (syntax-case head ()
    [(f arg ...) #'f]
    [(f arg ... . rest) #'f]
    [else (error "expected a head, got" head)]))

; Build a dispatcher, a procedure that takes a head and returns an expression.
; See test submodule for examples.
(define-syntax (def-dispatcher root-stx)
  (syntax-case root-stx ()
    [(_ id #:constant val)
     (syntax/loc root-stx
       (define-syntax id
         (dispatcher (λ (x) val))))]
    [(_ id [head body] ...)
     (with-syntax ([(proc-id ...) (map get-proc-id (syntax->list #'(head ...)))])
       (quasisyntax/loc root-stx
         (define-syntax id
           (dispatcher
            (λ (requested-head)
              ; It might be better not to use literals here, but it's too difficult for now
              (syntax-case requested-head (proc-id ...)
                [head #`body]
                ...
                [else (error "dispatcher has no match:" 'id (syntax->datum requested-head))]))))))]
    [else (error "TODO bvf014jl")]))

(define-syntax (define/weave stx)
  (syntax-case stx ()
    [(_ retval-dispatcher
        typecheck-dispatcher
        nullcheck-dispatcher
        head)
     (let* ([expand-body (syntax-local-value #'retval-dispatcher)]
            [check-types (syntax-local-value #'typecheck-dispatcher)]
            [check-null (syntax-local-value #'nullcheck-dispatcher)])
       #`(define head
           (let* ([return-type #,(check-types #'head)]
                  [nullability #,(check-null #'head)]
                  [result #,(expand-body #'head)])
             (>> result #:cast return-type #:null nullability))))]
    [else (error "TODO bfajkl352")]))
