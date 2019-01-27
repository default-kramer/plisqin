#lang racket
(require (submod "model.rkt" all)
         (only-in "core.rkt" get-src
                  [source make-source])
         (for-syntax racket syntax/parse))
(module+ test
  (require rackunit)
  (require "api.rkt"))

(provide def-table append! def/append!
         ; legacy compatibility:
         (rename-out [def/append! field-cases]
                     [def-table table]))

(define empty-proc (λ(arglist) nothing))

; Replaces (syntax else) with (syntax #t)
(define-syntax (de-else stx)
  (syntax-case stx (else)
    [(_ else) #'#t]
    [(_ x) #'x]))

; Appends more cond-like forms to the given appendable?
(define-syntax (append! stx)
  (syntax-parse stx
    [(append! (APPENDABLE args:id ...)
              [test-expr:expr then-body:expr]
              ...)
     #:declare APPENDABLE (expr/c #'appendable?)
     (begin
       (define arg-count (length (syntax->list #'(args ...))))
       #`(let ([a APPENDABLE.c])
           (define old-proc (appendable-proc a))
           (define new-proc
             (λ(arglist)
               (let ([result
                      (if (equal? (length arglist) #,arg-count)
                          (apply (λ(args ...)
                                   (cond
                                     [(de-else test-expr) then-body]
                                     ...
                                     [#t nothing]))
                                 arglist)
                          nothing)])
                 (if (nothing? result)
                     (old-proc arglist)
                     result))))
           (set-appendable-proc! a new-proc)))]))

; It would be better (I think) to use identifier-binding to know if something
; is already defined. But, that breaks schema.scrbl. Why?
(define-for-syntax fields-defined (make-hash))

; Same as append! except that if the appendable does not yet exist, it gets defined.
(define-syntax (def/append! stx)
  (syntax-parse stx
    [(def/append! (a args:id ...)
       [test-expr:expr then-body:expr]
       ...)
     (define maybe-define-a
       ;(if (not (identifier-binding #'a))
       ;    #'(define a (proc empty-proc (~a 'a)))
       ;    #'(void))
       (let ([hashkey (syntax->datum #'a)])
         (if (hash-has-key? fields-defined hashkey)
             #'(void)
             (begin
               (hash-set! fields-defined hashkey #t)
               #'(define a (proc empty-proc (~a 'a)))))))
     #`(begin
         #,maybe-define-a
         (append! (a args ...)
                  [test-expr then-body]
                  ...))]))

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

(define-syntax (def-table stx)
  (syntax-parse stx
    ; (table MyTable) -> (table MyTable #f)
    [(def-table ctor:id)
     #'(def-table ctor #f)]
    ; (table MyTable #f) -> (table MyTable #f #f)
    [(def-table ctor:id table-name)
     #'(def-table ctor table-name #f)]
    ; (table MyTable #f #f) -> (table MyTable #f #f MyTable?)
    [(def-table ctor:id table-name default-alias)
     (with-syntax ([tester? (make-tester #'ctor)])
       #'(def-table ctor table-name default-alias tester?))]
    [(def-table ctor:id TABLE-NAME DEFAULT-ALIAS tester?:id)
     #:declare TABLE-NAME (expr/c #'(or/c #f string?))
     #:declare DEFAULT-ALIAS (expr/c #'(or/c #f string?))
     #'(begin
         (define table-name (or TABLE-NAME.c
                                (format "~a" 'ctor)))
         (define default-alias (or DEFAULT-ALIAS.c
                                   (make-default-alias table-name)))
         (define ctor
           (table empty-proc table-name default-alias))
         ; Append a rule that says (MyTable) returns a source
         (append! (ctor)
                  [#t (make-source default-alias table-name)])
         ; Append a rule that says (MyTable "str") returns a source
         (append! (ctor alias)
                  [(string? alias)
                   (make-source alias table-name)])
         (define (tester? x)
           (cond [(or (query? x)
                      (join? x)
                      (binding? x))
                  (tester? (get-src x))]
                 [(table? x) (tester? (table-name x))]
                 [(source? x) (tester? (source-table x))]
                 [(string? x) (equal? x table-name)]
                 [else #f])))]))

(module+ test
  (def-table Foo)

  (define src (Foo))
  (check-true (source? src))
  (check-equal? (source-table src) "Foo")
  (check-equal? (source-alias src) "_foo")

  (set! src (Foo "asdf"))
  (check-true (source? src))
  (check-equal? (source-table src) "Foo")
  (check-equal? (source-alias src) "asdf")

  (check-true (and (Foo? (Foo))
                   (Foo? (Foo "f"))
                   (Foo? (from f Foo))
                   (Foo? (join f Foo))
                   (Foo? (join f "Foo")))))