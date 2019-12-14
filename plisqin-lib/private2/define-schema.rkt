#lang racket

(provide define-schema this)

(require (only-in morsel-lib gen:queryable get-queryable)
         (prefix-in %% (only-in plisqin-lib/unsafe/main scalar)))

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

(define-syntax (def-dispatch-proc stx)
  (syntax-case stx ()
    [(_ id [given-id given-body] ...)
     (syntax/loc #'id
       (define (id x)
         (syntax-parameterize ([this (λ (stx) #'x)])
           (let ([x-queryable (get-queryable x)])
             (cond
               [(equal? (get-queryable given-id) x-queryable) given-body]
               ...
               [else (raise-argument-error 'id (format "one of ~a" '(given-id ...)) x)])))))]))

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

  (define (add-table dict func-id)
    (let ([key (syntax-e func-id)])
      (dict-set dict key (:func func-id (list) #t))))

  ; func-id : identifier?
  ; cond-id : identifier?
  ; body    : syntax?
  (define (add dict func-id cond-id body)
    (let* ([key (syntax-e func-id)]
           [func (dict-ref dict key #f)]
           [found? func]
           [func (or func
                     (:func func-id (list) #f))]
           [func (struct-copy :func func
                              [conds (cons (:cond cond-id body)
                                           (:func-conds func))])])
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

  (define (find-tables dict stx)
    (syntax-case stx ()
      [((table id stuff ...) rest ...)
       (find-tables (add-table dict #'id)
                    #'(rest ...))]
      [() dict]))

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
      [(#:column id rest ...)
       (parse2 (add dict #'id cond-id #`(%%scalar this #,(format ".~a" (syntax-e #'id))))
               cond-id
               #'(#:column rest ...))]
      ; Automatically set join #:to if not set
      [(#:has-one [id (join a b c stuff ...)] rest ...)
       (not (kw? #'c))
       (parse2 dict cond-id
               #'(#:has-one [id (join a b #:to this c stuff ...)] rest ...))]
      [(keyword [id body] rest ...)
       ; TODO verify that it is a keyword we recognize
       (kw? #'keyword)
       (parse2 (add dict #'id cond-id #'body)
               cond-id
               #'(keyword rest ...))])))

; TODO need to use syntax-parse
(define-syntax (define-schema stx)
  (syntax-case stx ()
    [(_ schema-id clause ...)
     ; collect into association list
     (let* ([lst (find-tables (list) #'(clause ...))]
            [lst (parse0 lst #'(clause ...))]
            [funcs (map cdr lst)]
            [new-stx
             (quasisyntax/loc stx
               (begin
                 ; This is handy, but expansion takes too much time and space on large schemas
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
                 ; A normal procedure that works on symbols performs much better.
                 ; The caller must provide a quoted pattern like '(CategoryName Product)
                 (define (schema-id pattern)
                   (case pattern
                     #,@(flatten
                         (for/list ([func funcs])
                           (for/list ([cond (:func-conds func)])
                             #`[((#,(:func-id func) #,(:cond-id cond)))
                                (quote #,(:cond-expr cond))])))
                     [else 'undefined]))
                 #,@(for/list ([func funcs])
                      (quasisyntax/loc (:func-id func)
                        (#,(if (:func-table? func)
                               #'def-table
                               #'def-dispatch-proc)
                         #,(:func-id func)
                         #,@(map (λ(c) #`[#,(:cond-id c) #,(:cond-expr c)])
                                 (:func-conds func)))))))])
       new-stx)]))
