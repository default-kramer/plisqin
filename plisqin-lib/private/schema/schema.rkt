#lang racket

(provide define-schema this)

; TODO
; * Search this file for TODOs
; * Move `property?` into the main lib, and derive the rackunit check from it
; * Protect the macro entry point with proper use of syntax/parse stuff
; * Error if a pair of (table-id proc-id) is redefined? Or just override it?

(require (for-syntax syntax/parse)
         rackunit
         racket/stxparam
         (only-in "../api.rkt"
                  scalar RS join join? join-on bool scalar? bool? aggregate?)
         (only-in "../schema.rkt"
                  def-table def/append!)
         (prefix-in |.| "../operators.rkt"))

(define-syntax-parameter this
  (λ(stx)
    (raise-syntax-error 'plisqin "`this` used out of context" stx)))

; The strategy is that if we have
#;(define-schema id
    (table foo
           #:column a b
           #:property
           [total (+ (a this)
                     (b this))])
    (table bar
           #:column a b))
; We want to normalize and flatten that into a series of self-contained clauses.
; For example, the normalized form of the above would be
#;(define-schema id
    [#:table foo]
    [#:column foo a]
    [#:column foo b]
    [#:property foo [total (+ (a this)
                              (b this))]]
    [#:table bar]
    [#:column bar a]
    [#:column bar b])
; Each clause should have all the information we need to expand it.
; All other clauses other than [#:table table-id] will have the form
#;[#:keyword table-id form]


;;; clause->definition
; Expands a clause into the definition for the main module
(define-syntax (clause->definition stx)
  (syntax-parse (cadr (syntax->list stx))
    [(#:table table-id:id)
     #'(def-table table-id)]
    [(#:column table-id:id column-id:id)
     #`(def/append! (column-id x)
         [(#,(make-? #'table-id) x)
          (scalar x (RS #,(datum->syntax #'here
                                         (format ".~a" (syntax-e #'column-id)))))])]
    [(keyword table-id [join-table-id #:using join-column-id ...])
     #'(clause->definition
        (keyword table-id
                 [join-table-id (join other (join-table-id)
                                      (join-on (.= (join-column-id other)
                                                   (join-column-id this)))
                                      ...)]))]
    [((~or* #:has-one #:property) table-id:id [proc-id:id proc-body:expr])
     (with-syntax ([ooo (quote-syntax ...)])
       #`(def/append! (proc-id x)
           [(#,(make-? #'table-id) x)
            (syntax-parameterize ([this (λ(stx)
                                          (syntax-case stx ()
                                            [(any ooo)
                                             (syntax/loc stx
                                               (#%app any ooo))]
                                            [_ (syntax/loc stx x)]))])
              proc-body)]))]))

;;; clause->test
; Expands a clause into a test for the test submodule
(define-syntax (clause->test stx)
  (syntax-parse (cadr (syntax->list stx))
    [(#:property table-id [proc-id proc-body])
     (syntax/loc #'proc-body
       (check-property? (proc-id (table-id))))]
    [(#:property else ...)
     (error "plisqin internal error: unrecognized #:property clause:" stx)]
    [(#:has-one table-id [proc-id #:using column-id ...])
     (syntax/loc #'proc-id
       (check-join? (proc-id (table-id))))]
    [(#:has-one table-id [proc-id proc-body])
     (syntax/loc #'proc-body
       (check-join? (proc-id (table-id))))]
    [(#:has-one else ...)
     (error "plisqin internal error: unrecognized #:has-one clause:" stx)]
    [else #'(void)]))

; Custom checks for the test submodule:
(define-simple-check (check-join? x)
  (join? x))

; Let's say that a property must be a scalar, bool, or aggregate.
; All joins must be singular.
; Grouped joins must be balanced with enclosing aggregates.
(define-check (check-property? x)
  (unless (or (scalar? x)
              (bool? x)
              (aggregate? x))
    (fail-check "Value was not a scalar, bool, or aggregate."))
  (unless #t #;(all-joins-are-singular? x)
    (fail-check "Value contains a non-singular join."))
  (unless #t #;(groups-and-aggregates-are-balanced? x)
    (fail-check "Grouped joins and aggregates are unbalanced.")))

; Given `table-id` this will create `table-id?`
(define-for-syntax (make-? id)
  #;(-> identifier? identifier?)
  (let ([sym (string->symbol (format "~a?" (syntax-e id)))])
    (datum->syntax id sym id id)))

; In order to support appending to an existing schema, we will stash all the
; clauses into a syntax transformer that we can recognize.
(begin-for-syntax
  (struct xformer (proc clauses) #:transparent
    #:property prop:procedure 0))

; Someday we might want to also store the clauses for runtime usage
; (maybe generating scribble docs?) but for now the runtime value of
; a schema can be nothing.
(struct schema ())

;;; define-schema-from-clauses
; Expands a list of clauses into
; 1) definitions in the main module
; 2) tests in the test submodule
(define-syntax (define-schema-from-clauses stx)
  ; We need the [#:table x ...] clauses to come first, so build this sort proc
  (define (sorter stx1 stx2)
    (define (priority stx)
      ; stx is like (make-def/append [#:table a]) so we use cadr here:
      (syntax-case (cadr (syntax->list stx)) ()
        [(#:table stuff ...) 0]
        [else 1]))
    (< (priority stx1) (priority stx2)))
  (syntax-case stx ()
    [(_ id
        clause ...)
     (with-syntax ([ooo (quote-syntax ...)])
       #`(begin
           (define the-schema (schema))
           (define-syntax id
             (xformer (λ(stx)
                        (syntax-case stx ()
                          [(anything ooo)
                           (syntax/loc stx
                             (#%app anything ooo))]
                          [_ (syntax/loc stx the-schema)]))
                      (syntax->list #'(clause ...))))
           ; Generate definitions:
           #,(let* ([definitions (syntax->list #'((clause->definition clause)
                                                  ...))]
                    [definitions (sort definitions sorter)])
               #`(begin #,@definitions))
           ; Generate tests (unhygenic `test` here):
           ; TODO this fails on the REPL - should check for 'top-level vs 'module
           (module+ #,(datum->syntax stx 'test)
             #,@(syntax->list #'((clause->test clause)
                                 ...)))))]))


(begin-for-syntax

  ;;; make-clauses
  ; Given a syntax object like
  #;#'((table t1
              #:foo b [c d]
              #:bar e f)
       (table t2
              #:foo b c))
  ; We want to transform it to a list of potential clauses like
  #;(syntax->list #'([#:foo t1 b]
                     [#:foo t1 [c d]]
                     [#:bar t1 e]
                     [#:bar t1 f]
                     [#:foo t2 b]
                     [#:foo t2 c]))
  ; I say "potential" clauses because we don't actually verify that it matches
  ; one of the recognized clause forms. We simply look at the keyword structure.
  (define (make-clauses stx)
    #;(-> syntax? (listof syntax?))

    ; Accepts a syntax object representing the "contents of a table form beyond
    ; the table-id, for example:
    #;(#:column a b c #:property [d e])
    (define (helper table-id stx [keyword #f])
      #;(-> identifier? syntax? (or/c #f syntax?) (listof syntax?))
      (syntax-case stx ()
        [() (list)]
        [(kw rest ...)
         (keyword? (syntax-e #'kw))
         (helper table-id #'(rest ...) #'kw)]
        [(clause rest ...)
         (begin
           (when (not keyword)
             (raise-syntax-error 'define-schema "Expected a keyword" #'clause))
           (define new-clause
             (datum->syntax #'clause
                            (list keyword table-id #'clause)
                            #'clause
                            #'clause))
           (cons new-clause
                 (helper table-id #'(rest ...) keyword)))]))

    (syntax-case stx ()
      [() (list)]
      [((table-id x ...) rest ...)
       (append (helper #'table-id #'(x ...))
               (make-clauses #'(rest ...)))])))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ schema-id:id
        (~optional (~seq #:append-to append-id:id))
        ; TODO want literal table here, ignoring bindings
        (table table-id:id
               form
               ...)
        ...)
     ; If append-id was given, it must be bound to an xformer so we can pull
     ; its clauses out and reuse them.
     (let* ([has-append? (attribute append-id)]
            [xf (and has-append?
                     (syntax-local-value #'append-id (λ() #f)))]
            [_ (when (and has-append?
                          (not (xformer? xf)))
                 (raise-syntax-error 'define-schema "not an appendable schema" #'append-id))]
            [append-to-clauses (if has-append?
                                   (xformer-clauses xf)
                                   (list))])
       #`(define-schema-from-clauses schema-id
           [#:table table-id]
           ...
           #,@append-to-clauses
           #,@(make-clauses #'((table-id form ...)
                               ...))))]))
