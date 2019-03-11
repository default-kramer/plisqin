#lang racket
(require (except-in (submod "model.rkt" all)
                    raw-sql)
         (only-in "core.rkt"
                  scalar
                  attached-join?
                  get-src
                  raw-sql
                  [source make-source])
         (for-syntax racket syntax/parse))
(module+ test
  (require rackunit)
  (require "api.rkt"))

(provide def/append! append!
         def-table def-fields-of
         ; legacy compatibility:
         (rename-out [def/append! field-cases]
                     [def-table table]))

(define empty-proc (λ(arglist) nothing))

(define/contract (is-table? t x)
  (-> (or/c table? string?) any/c any/c)
  (if (table? t)
      (is-table? (table-name t) x)
      (cond [(or (query? x)
                 (join? x)
                 (attached-join? x))
             (is-table? t (get-src x))]
            [(table? x)
             (is-table? t (table-name x))]
            [(source? x)
             (is-table? t (source-table x))]
            [(string? x)
             (equal? t x)]
            [else #f])))

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

(define-syntax (def-fields-of stx)
  (syntax-parse stx
    [(_ TABLE fields:id ...)
     #:declare TABLE (expr/c #'(or/c string? table?))
     #`(begin
         (define table TABLE.c)
         (def/append! (fields x)
           [(is-table? table x)
            (scalar x (raw-sql (format ".~a" 'fields)))])
         ...)]))

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
     (hash-set! fields-defined (syntax->datum #'ctor) #t)
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
           (is-table? table-name x)))]))

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
                   (Foo? (join f "Foo"))))

  ; regression: def/append! can follow def-table
  (def-table table92)
  (def/append! (table92 a b c)
    [#t (list a b c)])

  (def-fields-of Foo bar)
  (check-not-false (bar (Foo))))
