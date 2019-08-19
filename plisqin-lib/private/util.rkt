#lang racket
(provide def-doc test flattenof build-guard-proc def/c)
(provide (all-from-out "new-way-to-walk.rkt"))
(provide (all-from-out "capture-syntax.rkt"))

(require (for-syntax syntax/strip-context))
(require "new-way-to-walk.rkt")
(require "capture-syntax.rkt")

; Use to define a scribble expression in the "docs" submodule.
; Importantly, we use replace-context so that when a scribble file calls this, the resulting
; syntax objects will be labeled as if they were "inline" in the scribble file.
(define-syntax-rule (def-doc name value)
  (module+ docs
    (define-syntax (name stx)
      (replace-context stx #'value))
    (provide name)))

; Use to define a private scope in the "test" submodule, to avoid name clashes.
(define-syntax-rule (test forms ...)
  (module+ test
    (let ([ignored 0])
      forms
      ...)))

(define/contract (flattenof element-contract)
  (-> contract? contract?)
  ; Per the docs on make-contract:
  ;   The proj argument is a curried function of two arguments: the first application accepts a blame object,
  ;   and the second accepts a value to protect with the contract.
  (define (projector blame)
    ; The flattener both flattens the list and enforces the contract on each element.
    (define (flattener elem)
      (match elem
        [(list (list nested ...) rest ...)
         (append (flattener nested) (flattener rest))]
        [(list val rest ...)
         ; TODO element-contract might not be a procedure.
         ; There must be something like (contract-apply element-contract val) I can use.
         (if (element-contract val)
             (cons val (flattener rest))
             (raise-blame-error (blame-add-context blame "an element of")
                                first
                                '(expected: "~a" given: "~e")
                                (contract-name element-contract) val))]
        [(list) (list)]
        [val
         (raise-blame-error blame
                            val
                            '(expected: "list?" given: "~v")
                            val)]))
    flattener)
  (make-contract #:name (list 'flattenof (contract-name element-contract))
                 #:first-order list?
                 #:projection projector
                 #:list-contract? #t))

; Produces a lambda to be used with #:guard in a struct definition.
(define-syntax-rule (build-guard-proc [field contract-expr] ...)
  (λ(field ... name)
    (values
     (let ([c contract-expr])
       (if (and c (contract? c))
           ; Just because c is a contract doesn't mean it is a procedure.
           ; So we have to use (contract c ...) to invoke it, catch the exception,
           ; and re-raise an argument error instead
           (with-handlers ([exn:fail:contract?
                            (λ(ex) (raise-argument-error
                                    (string->symbol (format "~a.~a" name 'field))
                                    (~v c)
                                    field))])
             (contract c field #f #f))
           field))
     ...)))

(define-syntax-rule (def/c head contract body ...)
  #;(define/contract head
      contract
      body ...)
  (define head
    body ...))
