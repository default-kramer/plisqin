#lang racket

(provide typed<%> typed? get-type assign-type
         type? define-types type-supertypes)

; Runtime representation of a type
; contract-proc : procedure?
; id            : symbol?
; supertypes    : (listof type?)
(struct type (contract-proc id supertypes flattened-supertypes)
  #:property prop:procedure 0
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (write-string (format "~a" (type-id me)) port))])

; An object that can have a type assigned to it
(define typed<%>
  (interface ()
    get-type      #;(-> this (or/c #f type?))
    assign-type   #;(-> this type? new-this)
    ))

(define typed? (is-a?/c typed<%>))

(define (get-type x)
  (and (typed? x)
       (send x get-type)))
(define (assign-type x type)
  (send x assign-type type))


(define (all-types x)
  (match x
    [(list a rest ...)
     (cons (all-types a)
           (all-types rest))]
    [(list) (list)]
    [else
     (cons x
           (all-types (type-supertypes x)))]))

(define-syntax (define-type stx)
  (syntax-case stx ()
    [(_ id supertype ...)
     (syntax/loc stx
       (begin
         (define my-supertypes (list supertype ...))
         ; Eagerly flatten the type hierarchy
         (define flattened-supertypes
           (remove-duplicates (flatten (all-types my-supertypes))))
         (define (contract-proc x)
           (let ([actual-type (get-type x)]
                 [lst (cons id flattened-supertypes)])
             (and actual-type
                  (or (eq? id actual-type)
                      (member id (type-flattened-supertypes actual-type) eq?))
                  #t)))
         (define id
           (type (procedure-rename contract-proc 'id)
                 'id my-supertypes flattened-supertypes))))]))

(define-syntax (define-types stx)
  (syntax-case stx ()
    [(_ [id supertype-id ...] ...)
     (syntax/loc stx
       (begin
         (define-type id supertype-id ...)
         ...))]))
