#lang racket
(provide def! def/append! append! via!
         def-table def-fields-of
         ; legacy compatibility:
         (rename-out [def/append! field-cases]
                     [def-table table]))

; TODO there is a bug here.
; We use identifier-binding plus a hash on the side to track whether something
; has been defined. But the hash causes a problem if you do
#;(begin
    (let ()
      (def/append! (blah) [#t 'blah]))
    (let ()
      (def/append! (blah) [#t 'blah])))
; because the hash says that `blah` is already defined, but it's not in scope.

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
    [(def/append! (a:id args:id ...)
       [test-expr:expr then-body:expr]
       ...)
     (define maybe-define-a
       (let ([hashkey (syntax->datum #'a)])
         ; Need to check both our hash and identifier-binding
         (if (or (hash-has-key? fields-defined hashkey)
                 (identifier-binding #'a))
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


; An "arg spec" is an identifier in which a double-colon has meaning.
; If it is of the form `arg-id::arg-type` then we split it into `arg-id` and `arg-type`.
; Otherwise it is simply `arg-id` with no type.
(module split-helper racket
  (provide get-arg-id get-arg-type)

  (define (string->stx str stx offset)
    (datum->syntax stx
                   (string->symbol str)
                   (list (syntax-source stx)
                         (syntax-line stx)
                         (syntax-column stx)
                         (+ offset (syntax-position stx))
                         (string-length str))
                   stx))

  (define (split-arg-spec spec)
    (define result (string-split (~a (syntax-e spec)) "::"))
    (match (length result)
      [0 (cons spec #f)]
      [1 (cons spec #f)]
      [2 (let* ([str1 (car result)]
                [str2 (cadr result)]
                [stx1 (string->stx str1 spec 0)]
                [stx2 (string->stx str2 spec (+ 2 (string-length str1)))])
           (cons stx1 stx2))]
      [else (raise-syntax-error #f "invalid argument spec (too many colons)" spec)]))

  (define (get-arg-id spec)
    (car (split-arg-spec spec)))
  (define (get-arg-type spec)
    (cdr (split-arg-spec spec))))

(require (for-syntax 'split-helper))

(module+ test
  (require (submod ".." split-helper))

  (define orig (read-syntax "src" (open-input-string "foo::barium")))
  (check-equal? (syntax-position orig) 1)
  (check-equal? (syntax-span orig) 11)

  (define arg-id (get-arg-id orig))
  (define arg-type (get-arg-type orig))
  (check-equal? (syntax-position arg-id) 1)
  (check-equal? (syntax-span arg-id) 3)
  (check-equal? (syntax-position arg-type) 6)
  (check-equal? (syntax-span arg-type) 6))

(define (validate-type type stx)
  (when (not (or (table? type)
                 (contract? type)))
    ; Is it better to use `raise-syntax-error` or just `error` here?
    (raise-syntax-error #f "invalid type name (must be table or predicate)" stx)))

(define (check-arg arg type)
  (if (table? type)
      (is-table? type arg)
      ; else just assume its a predicate (this might not be safe)
      (type arg)))

(define-for-syntax (change-srcloc stx new-loc)
  (datum->syntax stx (syntax-e stx) new-loc stx))


; Given
#;(validate-types (arg-spec ...))
; we generate approximately
#;(begin (validate-type arg-type #'arg-type) ...)
; while being aware that `arg-type` is an optional part of `arg-spec`.
; The goal is report a "type error" as soon as possible.
(define-syntax (validate-types stx)
  (syntax-case stx ()
    [(_ (specs ...))
     #`(begin
         #,@(filter
             identity
             (for/list ([spec (syntax->list #'(specs ...))])
               (let ([result (get-arg-type spec)])
                 (and result
                      (change-srcloc #`(validate-type #,result #'#,result)
                                     result))))))]))


; Given
#;(def-help (proc arg-spec ...) body ...)
; we generate approximately
#;(def/append! (proc arg-id ...)
    [(and (check-arg arg-id arg-type)
          ...)
     body ...])
; while being aware that `arg-type` is an optional part of `arg-spec`.
(define-syntax (def-help stx)
  (syntax-case stx ()
    [(_ (proc (specs ...)) body ...)
     #`(def/append! (proc #,@(map get-arg-id (syntax->list #'(specs ...))))
         [(and #,@(filter
                   identity
                   (for/list ([spec (syntax->list #'(specs ...))])
                     (let ([result (get-arg-type spec)])
                       (and result
                            #`(check-arg #,(get-arg-id spec) #,result))))))
          (begin
            body ...)])]))

(define-syntax (def! stx)
  (syntax-parse stx
    [(_ (proc:id arg-spec:id ...)
        body:expr ...+)
     #`(begin
         (validate-types (arg-spec ...))
         (def-help (proc (arg-spec ...))
           body ...))]))

(module+ test
  (def! (bar)
    'nada)
  (def! (bar str::string?)
    (format "~a ~a" str str))
  (def! (bar x::number?)
    (* 2 x))
  (check-equal? (bar)
                'nada)
  (check-equal? (bar "pizza")
                "pizza pizza")
  (check-equal? (bar 21)
                42)

  ; we should be able to use both `TableFoo` and `TableFoo?` as a type
  (def-table TableFoo)
  (define foo (TableFoo))

  (def! (bar x)
    `(simply ,x))
  (def! (bar x::TableFoo)
    `(got-foo ,x))
  (check-equal? (bar 1)
                '(simply 1))
  (check-equal? (bar foo)
                `(got-foo ,foo))
  (let ([q {from f TableFoo}])
    (check-equal? (bar q)
                  `(got-foo ,q)))

  (def! (bar x::TableFoo?)
    `(got-foo-again ,x))
  (check-equal? (bar 1)
                '(simply 1))
  (check-equal? (bar foo)
                `(got-foo-again ,foo)))


; via!
(define-syntax-rule (via-one! proc owner [target ...])
  (append! (proc arg)
           [(check-arg arg target)
            (proc (owner arg))]
           ...))

(define-syntax (via! stx)
  (syntax-parse stx
    [(_ owner:id #:link target:id ...+ #:to proc:id ...+)
     #'(begin
         (via-one! proc owner [target ...])
         ...)]))

(module+ test
  (def-table City)
  (def-table Country)
  (def-fields-of City
    CityId CountryId CityName)
  (def-fields-of Country
    CountryId CountryName CountryPop)

  (define fake-join (Country "pretend-this-is-a-join"))
  (def/append! (Country x)
    [(City? x)
     fake-join])

  (via! Country
        #:link City
        #:to CountryName CountryPop)

  (check-equal? (CountryName (City))
                (CountryName fake-join))
  (check-equal? (CountryPop (City))
                (CountryPop fake-join)))
