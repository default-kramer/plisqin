#lang racket

(provide define-schema this table?)

(require (only-in "_core.rkt" gen:queryable get-queryable)
         (only-in "sql/fragment.rkt" fragment? >>)
         (submod "from.rkt" define-schema-helper)
         (for-syntax syntax/parse)
         "_null.rkt"
         "_types.rkt"
         (submod "_null.rkt" more)
         (prefix-in %% (only-in (submod "./sql/frags.rkt" unsafe)
                                scalar sql)))

(require racket/stxparam
         (for-syntax racket/list))

(define-syntax-parameter this
  (λ (stx) (raise-syntax-error 'this "used out of context (Plisqin's this)" stx)))

(begin-for-syntax
  (define-syntax-class column-spec
    (pattern (~or* column-id:id
                   [column-id:id (~optional (~seq #:as asname:expr))
                                 (~optional (~seq #:type Type:expr))
                                 (~optional (~seq #:null nullability:expr))
                                 (~optional (~seq #:dbname dbname:expr))])))
  (define-syntax-class proc-spec
    (pattern [proc-id:id body:expr]))
  (define-splicing-syntax-class item-spec
    (pattern (~or* (~seq #:column col:column-spec ...)
                   (~seq #:property item:proc-spec ...)
                   (~seq #:has-one item:proc-spec ...)
                   (~seq #:has-group item:proc-spec ...))))
  (define-syntax-class table-spec
    #:datum-literals (table)
    (pattern (table table-id:id item:item-spec ...))))

(struct table (proc name) #:transparent
  #:property prop:procedure 0
  #:property prop:instance #t
  #:property prop:trusted-queryable #t
  #:methods gen:queryable
  [(define (unwrap-queryable me)
     (table-name me))]
  #:methods gen:custom-nullability
  [(define (get-custom-nullability me)
     no)]
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (write-string (table-name me) port))])

(define (do-column this column-sym table-sym
                   as-name type nullability dbname)
  ; Using macro contracts increases code size by about 4x, so compromise and
  ; do the validation here instead.
  (define (validate value contract keyword)
    (when (not (contract value))
      (raise-arguments-error
       'define-schema
       "contract violation (noticed late, backtrace might be misleading)"
       "table" table-sym
       "column" column-sym
       "keyword" keyword
       "expected" contract
       "given" value)))
  (validate as-name (or/c symbol? string?) '#:as)
  ; because `(or/c type?)` prints better than `type?`
  (validate type (or/c type?) '#:type)
  (validate nullability (or/c #f nullability?) '#:null)
  (validate dbname (or/c symbol? string?) '#:dbname)

  (define a (%%sql "." dbname))
  ; The nullability of the entire expression will be inferred from
  ; whatever `this` is plus this fragment:
  (define b (if nullability
                (>> a #:null nullability)
                a))
  (define c (%%scalar this b))
  (>> c #:cast type #:as as-name))

(define-syntax (handle-column stx)
  (syntax-parse stx
    [(_ table-id:id col:column-spec)
     (quasisyntax/loc stx
       (do-column this 'col.column-id 'table-id
                  (~? col.asname 'col.column-id)
                  (~? col.Type Scalar?)
                  (~? col.nullability #f)
                  (~? col.dbname 'col.column-id)))]))

(define-syntax (handle-property stx)
  (syntax-parse stx
    [(_ [id:id body:expr])
     (quasisyntax/loc stx
       (>> body #:as 'id))]))

; Given that `(CategoryName Category)` and `(CategoryName Product)` are both defined
; via define-schema, this builds the procedure `(CategoryName x)` which uses cond
; to choose the correct body based on `(get-queryable x)`
(define-syntax (def-dispatch-proc stx)
  (syntax-case stx ()
    [(_ proc-id [table-id cond-body] ...)
     (syntax/loc #'proc-id
       (define (proc-id x)
         (syntax-parameterize ([this (λ (stx) (syntax-case stx ()
                                                [a (identifier? #'a) #'x]
                                                [_ (raise-syntax-error
                                                    'this "cannot be used as a procedure" stx)]))])
           (let ([x-queryable (get-queryable x)])
             (cond
               [(equal? (get-queryable table-id) x-queryable) cond-body]
               ...
               [else (raise-argument-error 'proc-id (format "one of ~a" '(table-id ...)) x)])))))]))

(define-for-syntax (unintern-id id)
  (datum->syntax id (string->uninterned-symbol (format "~a" (syntax-e id))) id #f))

(define-syntax (def-table stx)
  (syntax-case stx ()
    [(_ id [given-id given-body] ...)
     (with-syntax ([proc-id (unintern-id #'id)])
       (syntax/loc #'id
         (begin
           (def-dispatch-proc proc-id
             [given-id given-body] ...)
           (define id (table proc-id (~a 'id))))))]))

(begin-for-syntax
  (require racket/dict)

  ; A function has many conditions. A condition has only one expression.
  ; We will build a dict of symbol => :func
  (struct :func (id conds table?) #:transparent)
  (struct :cond (id expr) #:transparent)

  ; We are also going to collect a big datum for metadata purposes.
  (define meta-datum-collector (make-parameter (list)))

  ; func-id : identifier?
  (define (add-table dict func-id)
    (let ([key (syntax-e func-id)])
      (meta-datum-collector (cons `(#:table ,(syntax->datum func-id))
                                  (meta-datum-collector)))
      (dict-set dict key (:func func-id (list) #t))))

  ; This is a first pass that just collects all the table-ids.
  (define (find-tables dict stx)
    (syntax-parse stx
      [(id:id rest ...)
       (find-tables (add-table dict #'id)
                    #'(rest ...))]
      [() dict]
      [else (error "Plisqin assert fail: find-tables")]))

  ; func-id : identifier?
  ; cond-id : identifier?
  ; body    : syntax?
  (define (add dict func-id cond-id body)
    (let* ([orig-body body]
           [body body]
           [key (syntax-e func-id)]
           [func (dict-ref dict key #f)]
           [found? func]
           [func (or func
                     (:func func-id (list) #f))]
           [func (struct-copy :func func
                              [conds (cons (:cond cond-id body)
                                           (:func-conds func))])])
      (meta-datum-collector (cons `(#:cond ,(syntax->datum #`(#,func-id #,cond-id)))
                                  (meta-datum-collector)))
      (dict-set dict key func)))

  (define (parse0 dict stx)
    (syntax-case stx ()
      [(clause rest ...)
       (parse0 (parse1 dict #'clause)
               #'(rest ...))]
      [() dict]))

  (define (parse1 dict stx)
    (syntax-case stx ()
      [()
       dict]
      [(table id)
       dict]
      [(table id clause ...)
       (parse2 dict #'id #'(clause ...))]))

  (define (kw? x)
    (keyword? (syntax-e x)))

  (define (parse2 dict cond-id stx)
    (syntax-case stx ()
      ; one remaining keyword is our base case
      [(a)
       (kw? #'a)
       dict]
      ; a keyword followed by another keyword - skip the first one
      [(a b rest ...)
       (and (kw? #'a)
            (kw? #'b))
       (parse2 dict cond-id #'(b rest ...))]
      [(#:column [id options ...] rest ...)
       (parse2 (add dict #'id cond-id #`(handle-column #,cond-id [id options ...]))
               cond-id
               #'(#:column rest ...))]
      [(#:column id rest ...)
       (parse2 dict cond-id #'(#:column [id] rest ...))]
      ; Automatically set join #:to if not set
      [(join-kw [id (join a b c stuff ...)] rest ...)
       (and (not (kw? #'c))
            (member (syntax-e #'join-kw) '(#:has-one #:has-group)))
       (parse2 dict cond-id
               #'(join-kw [id (join a b #:to this c stuff ...)] rest ...))]
      [(keyword [id body] rest ...)
       (member (syntax-e #'keyword) '(#:has-one #:has-group #:property))
       (parse2 (add dict #'id cond-id
                    (case (syntax-e #'keyword)
                      [(#:property)
                       #'(handle-property [id body])]
                      [else #'body]))
               cond-id
               #'(keyword rest ...))]
      [else
       (raise-syntax-error 'define-schema "something went wrong..." stx)]))
  )

; Creates a procedure that searches the meta-datums for '(#:cond (ProcSym x)) and returns x.
(define (proc-matcher ProcSym)
  (λ (form)
    (match form
      [(list #:cond (list f x))
       #:when (equal? f ProcSym)
       x]
      [else #f])))

; Creates a procedure that searches the meta-datums for '(#:cond (f TableSym)) and returns f.
(define (table-matcher TableSym)
  (λ (form)
    (match form
      [(list #:cond (list f x))
       #:when (equal? x TableSym)
       f]
      [else #f])))

; Applies the result of `proc-matcher` or `table-matcher`
(define (search meta-datums matcher)
  (sort (filter identity (map matcher meta-datums)) symbol<?))

(define (get-tables meta-datums)
  (let* ([tables (map (λ (form)
                        (match form
                          [(list #:table Table)
                           Table]
                          [else #f]))
                      meta-datums)]
         [tables (filter identity tables)])
    (sort tables symbol<?)))

(define (make-schema-function meta-datums)
  (lambda (pattern)
    (match pattern
      [(list '_ '_) meta-datums]
      ['tables
       (get-tables meta-datums)]
      [(list '_ TableSym)
       (search meta-datums (table-matcher TableSym))]
      [(list ProcSym '_)
       (search meta-datums (proc-matcher ProcSym))]
      [else (error "invalid pattern:" pattern)])))

; stx : the original (define-schema ....) syntax object
; table-ids : a syntax object like (T1 T2 ...) for each table id
(define-for-syntax (*define-schema stx table-ids)
  (syntax-case stx ()
    [(_ schema-id clause ...)
     (let-values ([(lst tables meta-datums)
                   (parameterize ([meta-datum-collector (list)])
                     ; collect into association list
                     (let* ([lst (find-tables (list) table-ids)]
                            ; make `tables` a list of symbols, one for each table
                            [tables (map car lst)]
                            [tables (sort tables symbol<?)]
                            [lst (parse0 lst #'(clause ...))])
                       (values lst tables (meta-datum-collector))))])
       (let* ([funcs (map cdr lst)]
              [new-stx
               (quasisyntax/loc stx
                 (begin
                   ; This might be more Racket-y, but expansion takes
                   ; too much time and space on large schemas
                   #;(define-syntax (schema-id stx)
                       (syntax-case stx (#,@(map :func-id funcs))
                         #,@(flatten
                             (for/list ([func funcs])
                               (for/list ([cond (:func-conds func)])
                                 (quasisyntax
                                  [(_ (#,(:func-id func) #,(:cond-id cond)))
                                   #'(quote #,(:cond-expr cond))]))))
                         [(_ (a b))
                          #'(quote undefined)]
                         ; I guess that this was the perf-killer, but disabling it doesn't
                         ; seem to make a noticable difference:
                         #;[(_ a)
                            #'(begin #,@(for/list ([func funcs])
                                          #`(let ([body (schema-id (#,(:func-id func) a))])
                                              (if (equal? 'undefined body)
                                                  (void)
                                                  (begin
                                                    (displayln '(#,(:func-id func) this))
                                                    (display "  => ")
                                                    (writeln body))))))]))
                   ; Define the metadata/schema function.
                   ; A normal procedure that works on symbols performs much better.
                   ; Especially if we just define the big datum once to keep the size
                   ; of the macro expansion as small as possible.
                   (define raw-forms (quote #,meta-datums))
                   (define schema-id (make-schema-function raw-forms))

                   ; Define the functions
                   #,@(for/list ([func funcs])
                        (quasisyntax/loc (:func-id func)
                          (#,(if (:func-table? func)
                                 #'def-table
                                 #'def-dispatch-proc)
                           #,(:func-id func)
                           #,@(map (λ(c) #`[#,(:cond-id c) #,(:cond-expr c)])
                                   (:func-conds func)))))))])
         new-stx))]))

(define-syntax (define-schema stx)
  ; This just layers syntax-parse over *define-schema
  (syntax-parse stx
    [(_ meta-proc-id:id t:table-spec ...)
     (let ([result (*define-schema stx #'(t.table-id ...))])
       result)]))


(module+ test
  (require rackunit
           (only-in "from.rkt" from instance? instanceof)
           (only-in "_core.rkt" to-sql)
           plisqin-lib/unsafe)

  (define-schema $$
    (table A
           #:column
           [Foo #:null no]
           [Bar #:null yes])
    (table B
           #:column
           Bar
           Baz
           [Carrot #:as "Carat" #:dbname "car_rot"]
           #:property
           [Carrot2
            (Carrot this)]
           )
    ; make sure this is allowed:
    (table EmptyTable)
    )

  (check-equal? ($$ 'tables)
                '(A B EmptyTable))
  (check-equal? ($$ '(_ A))
                '(Bar Foo))
  (check-equal? ($$ '(_ B))
                '(Bar Baz Carrot Carrot2))
  (check-equal? ($$ '(Foo _))
                '(A))
  (check-equal? ($$ '(Bar _))
                '(A B))

  (let ([q (from b B
                 (select (Carrot b)))])
    (check-equal? (to-sql q) #<<HEREDOC
select b.car_rot as Carat
from B b
HEREDOC
                  ))

  ; The as-name is automatically set on each #:property.
  ; If this is not desired, maybe allow
  #;[Carrot2
     #:as (or 'something-else #f)
     (Carrot this)]
  ; But for now we don't need it
  (let ([q (from b B
                 (select (Carrot2 b)))])
    (check-equal? (to-sql q) #<<HEREDOC
select b.car_rot as Carrot2
from B b
HEREDOC
                  ))

  ; some tests of #:null
  (check-equal? (nullability A)
                no)
  (check-equal? (nullability (Foo A))
                no)
  (check-equal? (nullability (Bar A))
                yes)
  (check-equal? (nullability (Bar B))
                maybe)

  ; tables are instances
  (check-pred instance? A)
  (check-pred instance? B)
  (check-pred (instanceof A) A)
  (check-pred (instanceof B) B)
  )
