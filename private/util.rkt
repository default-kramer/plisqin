#lang racket
(provide def-doc test flattenof)
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
(provide def-doc)

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