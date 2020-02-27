#lang racket

(provide define-schema this)

(require (only-in morsel-lib gen:queryable get-queryable)
         (only-in "sql/fragment.rkt" fragment? >>)
         (for-syntax syntax/parse)
         "_null.rkt"
         (prefix-in %% (only-in (submod "./sql/frags.rkt" unsafe)
                                scalar sql)))

(require racket/stxparam
         (for-syntax racket/list))

(define-syntax-parameter this
  (λ (stx) (raise-syntax-error 'this "used out of context (Plisqin's this)" stx)))

(struct table (proc name) #:transparent
  #:property prop:procedure 0
  #:methods gen:queryable
  [(define (unwrap-queryable me)
     (table-name me))]
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (write-string (table-name me) port))])

(define-syntax (handle-column stx)
  (syntax-parse stx
    [(_ [id:id (~optional (~seq #:as asname))
               (~optional (~seq #:type Type))
               (~optional (~seq #:null nullability))
               (~optional (~seq #:dbname dbname))])
     (quasisyntax/loc stx
       (>> (%%scalar this
                     ; The nullability of the entire expression will be inferred from
                     ; whatever `this` is plus this fragment:
                     (>> (%%sql (~? (~@ "." dbname)
                                    ; TODO can leave 'id as a symbol once Morsel can handle it
                                    (~@ "." (~a 'id))))
                         (~? (~@ #:null nullability)
                             (~@))))
           (~? (~@ #:cast Type)
               (~@))
           (~? (~@ #:as asname)
               (~@ #:as 'id))))]))

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
             ; TODO [id this] means that (Product x) will return x if x is already a Product.
             ; But do we want this? I think we used to, but not anymore...
             #;[id this]
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
      (meta-datum-collector (cons `(,(syntax->datum func-id) => #:table)
                                  (meta-datum-collector)))
      (dict-set dict key (:func func-id (list) #t))))

  ; This is a first pass that just collects all the table-ids.
  (define (find-tables dict stx)
    (syntax-case stx ()
      [((table id stuff ...) rest ...)
       (find-tables (add-table dict #'id)
                    #'(rest ...))]
      [() dict]))

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
      (meta-datum-collector (cons (list (syntax->datum #`(#,func-id #,cond-id))
                                        '=>
                                        (syntax->datum orig-body))
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
       (parse2 (add dict #'id cond-id #'(handle-column [id options ...]))
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
       ; TODO verify that it is a keyword we recognize
       (kw? #'keyword)
       (parse2 (add dict #'id cond-id #'body)
               cond-id
               #'(keyword rest ...))])))

; Creates a procedure that searches the meta-datums for '(ProcSym x) and returns x.
(define (proc-matcher ProcSym)
  (λ (form)
    ; form is something like '((f x) => body)
    (match (car form)
      [(list a b)
       #:when (equal? a ProcSym)
       b]
      [else #f])))

; Creates a procedure that searches the meta-datums for '(x TableSym) and returns x.
(define (table-matcher TableSym)
  (λ (form)
    ; form is something like '((f x) => body)
    (match (car form)
      [(list a b)
       #:when (equal? b TableSym)
       a]
      [else #f])))

; Applies the result of `proc-matcher` or `table-matcher`
(define (search meta-datums matcher)
  (sort (filter identity (map matcher meta-datums)) symbol<?))

(define (get-tables meta-datums)
  (let* ([tables (map (λ (form)
                        (match form
                          [(list Table '=> '#:table)
                           Table]
                          [else #f]))
                      meta-datums)]
         [tables (filter identity tables)])
    (sort tables symbol<?)))

(define (make-schema-function meta-datums)
  (lambda (pattern)
    (match pattern
      ['(_ _) meta-datums]
      ['tables
       (get-tables meta-datums)]
      [(list '_ TableSym)
       (search meta-datums (table-matcher TableSym))]
      [(list ProcSym '_)
       (search meta-datums (proc-matcher ProcSym))]
      [else (error "invalid pattern:" pattern)])))

; TODO need to use syntax-parse
(define-syntax (define-schema stx)
  (syntax-case stx ()
    [(_ schema-id clause ...)
     (let-values ([(lst tables meta-datums)
                   (parameterize ([meta-datum-collector (list)])
                     ; collect into association list
                     (let* ([lst (find-tables (list) #'(clause ...))]
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


(module+ test
  (require rackunit
           (only-in morsel-lib from)
           (only-in morsel-lib/sql to-sql)
           plisqin-lib/unsafe)

  (define-schema $$
    (table A
           #:column
           Foo
           Bar)
    (table B
           #:column
           Bar
           Baz
           [Carrot #:as "Carat" #:dbname "car_rot"]))

  (check-equal? ($$ 'tables)
                '(A B))
  (check-equal? ($$ '(_ A))
                '(Bar Foo))
  (check-equal? ($$ '(_ B))
                '(Bar Baz Carrot))
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
                  )))
