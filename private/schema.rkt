#lang racket
(require "core.rkt"
         (for-syntax racket
                     syntax/parse))
(module+ test
  (require rackunit)
  (require "api.rkt"))

(provide table field-cases)

(define (is-table? src table-name)
  (equal? (s:source-table src) table-name))

(define/contract (make-default-alias table-name)
  (-> string? string?)
  (format "_~a"
          (if (< (string-length table-name) 3)
              table-name
              (let* ([chars (string->list table-name)]
                     [head (char-downcase (car chars))])
                (list->string (cons head (cdr chars)))))))

(begin-for-syntax
  (define/contract (make-tester table-id)
    (-> syntax? syntax?)
    (datum->syntax
     table-id
     (string->symbol (format "~a?" (syntax->datum table-id))))))

(define-syntax (table stx)
  (syntax-parse stx
    ; (table MyTable) -> (table MyTable #f)
    [(table ctor:id)
     #`(table ctor #f)]
    ; (table MyTable #f) -> (table MyTable #f #f)
    [(table ctor:id table-name)
     #'(table ctor table-name #f)]
    ; (table MyTable #f #f) -> (table MyTable #f #f MyTable?)
    [(table ctor:id table-name default-alias)
     (with-syntax ([tester? (make-tester #'ctor)])
       #'(table ctor table-name default-alias tester?))]
    [(table ctor:id TABLE-NAME DEFAULT-ALIAS tester?:id)
     #:declare TABLE-NAME (expr/c #'(or/c #f string?))
     #:declare DEFAULT-ALIAS (expr/c #'(or/c #f string?))
     #'(begin
         (define table-name (or TABLE-NAME.c
                                (format "~a" 'ctor)))
         (define default-alias (or DEFAULT-ALIAS.c
                                   (make-default-alias table-name)))
         (define/contract (ctor [alias default-alias])
           (->* () (string?) source?)
           (source (or alias default-alias) table-name))
         (define (tester? x)
           (cond [(query? x) (tester? (s:query-source x))]
                 [(join? x) (tester? (s:join-query x))]
                 [(binding? x) (tester? (s:binding-join x))]
                 [(source? x) (is-table? x table-name)]
                 [else #f])))]))

; A field is just a symbol, like 'LOCATION_CODE
(define field? symbol?)

; something that can own a field
(define (owner? x) (or (query? x) (source? x) (join? x)))

; the result of a field lookup
;(define-type Resolution SqlToken)

; Assuming that we already know which field we are talking about, the resolver
; is a function from source to SqlToken (because the same field might be spelled
; differently based on which source we are referencing).
;(define-type FieldResolver (-> Owner (U Resolution #f)))

; Mapping of field to resolver.
; (: dispatch-table (Mutable-HashTable Field FieldResolver))
(define dispatch-table (make-hash))

;(: field-error (-> Field Owner Resolution))
(define (field-error fieldname owner)
  ;(error "Bad field lookup:" fieldname 'given (get-src owner))
  (println (format "WARNING! Bad field lookup: ~a.~a" (s:source-table (get-src owner)) fieldname))
  (sql (format "~a" fieldname)))

;(: make-default-resolver (-> Field FieldResolver))
(define (make-default-resolver field)
  (lambda (src) #f))

;(: get-resolver (-> Field FieldResolver))
(define (get-resolver field)
  (hash-ref dispatch-table field (lambda() (make-default-resolver field))))

;(: dispatch (-> Field Owner Resolution))
(define (dispatch field owner)
  (let* ([resolver (get-resolver field)])
    (or (resolver owner) (field-error field owner))))

; This updates the resolver for the given field to be
;   (λ(src) (or (new-resolver src) (existing-resolver src)))
; Meaning that if the new resolver returns non-false, it overrides the existing resolver.
; Otherwise it falls through. It kind of behaves like cond, but read bottom-to-top instead.
; I think this is what we want... you could have your schema as-is in one module, then override
; any definitions after you require it.
;(: add-resolver (-> Field FieldResolver Void))
(define (add-resolver field func)
  (let* ([existing (get-resolver field)]
         [new (λ(owner) (or (func owner) (existing owner)))])
    (hash-set! dispatch-table field new)))

; This keeps track (during macro expansion) of what fields we have already defined.
; This allows field-case to specify multiple conditions for the same field, but only
; the first occurrence will generate a (define ...) thus avoiding duplicates.
(define-for-syntax fields-defined (make-hash))

(define-syntax (field-case stx)
  (syntax-case stx ()
    [(field-case (field src) predicate result)
     (let ([hashkey (syntax->datum #'field)])
       (if (hash-has-key? fields-defined hashkey)
           #'(add-field-case (field src) predicate result)
           (begin
             (hash-set! fields-defined hashkey #t)
             #'(define-field-case (field src) predicate result))))]))

(define-syntax-rule (define-field-case (field src) predicate result)
  (begin
    ; The definition of (FieldName x) is always (dispatch 'FieldName x)
    (define (field src) (dispatch 'field src))
    (add-field-case (field src) predicate result)))

(define-syntax-rule (add-field-case (field src) predicate result)
  (add-resolver 'field (λ(src) (if predicate result #f))))

(define-syntax-rule (field-cases (field src) (predicate result) ...)
  (begin
    (field-case (field src) predicate result)
    ...))