#lang racket

(provide guard)

; Wraps the given procedure with argument validation. Expands
#;(guard blah
         [integer? integer? -> any]
         [string? string? -> any])
; to something like
#;(lambda arglist
    (match arglist
      [(list a b)
       #:when (and (integer? a)
                   (integer? b))
       (blah a b)]
      [(list a b)
       #:when (and (string? a)
                   (string? b))
       (blah a b)]
      [else (raise-argument-error stuff...)]))
; My testing has shown that this performs faster than `->` which is the fastest
; of the built-in procedure contract constructors.
; TODO still need to handle the return type though.

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-rule (matches-any? a b ...)
    (syntax-parse a
      [b #t]
      ...
      [else #f]))

  (define-syntax-class rest-modifier
    #:description "`...` or `...+`"
    (pattern (~or (~datum ...)
                  (~datum ...+))))

  (define-syntax-class contract-expr
    (pattern c:expr
             #:fail-when (matches-any? #'c
                                       a:rest-modifier
                                       (~datum ->))
             (format "`~a` cannot be used as a contract" (syntax-e #'c))))

  (define-syntax-class contract-spec
    #:description "a contract-spec similar to [foo? bar? -> baz?]"
    (pattern [arg/c:contract-expr
              ...
              (~optional (~seq rest/c:contract-expr
                               rest-mod:rest-modifier))
              (~datum ->)
              return/c:contract-expr]))

  ;;; spec->match-clause
  ; spec   : syntax?
  ; return : (-> syntax? (or/c #f (listof identifier?)) syntax?)
  ;
  ; Given a spec like [foo? bar? -> baz?] expand to something like
  #;[(list a b)
     #:when (and (foo? a)
                 (bar? b))
     (return baz? (a b))]
  ; If there is a rest argument, we (return baz? #f) instead.
  (define (spec->match-clause spec return)
    (syntax-parse spec
      [s:contract-spec
       (let* ([rest-mod (syntax->list #'((~? s.rest-mod)))]
              [rest-mod (and (not (null? rest-mod))
                             (car rest-mod))]
              [has-rest? rest-mod]
              [has...+ (and has-rest?
                            (equal? (syntax-e rest-mod) '...+))])
         (with-syntax ([ooo (quote-syntax ...)]
                       [(arg-id ...) (generate-temporaries #'(s.arg/c ...))]
                       [(rest-id) (generate-temporaries '(rest))])
           (quasisyntax/loc spec
             [(list arg-id ...
                    #,@(if has-rest?
                           (list #'rest-id #'ooo)
                           (list)))
              #:when (and #,@(map (λ (arg/c arg-id) (quasisyntax/loc arg/c
                                                      (#,arg/c #,arg-id)))
                                  (syntax->list #'(s.arg/c ...))
                                  (syntax->list #'(arg-id ...)))
                          #,@(if has-rest?
                                 (list #'(andmap s.rest/c rest-id))
                                 (list))
                          #,@(if has...+
                                 (list #'(pair? rest-id)) ; ensure the list is not empty
                                 (list)))
              #,(return #'s.return/c
                        (if has-rest?
                            #f
                            (syntax->list #'(arg-id ...))))])))])))

(define (format-specs specs)
  (string-join (map ~a specs) "\n"))

(define-syntax (guard stx)
  (syntax-parse stx
    [(_ func:id spec ...+)
     (syntax/loc stx
       (guard #:name func func spec ...))]
    [(_ #:name name:id func:id spec ...+)
     (with-syntax ([arglist #'arglist])
       (let ([return (λ (return/c arg-ids)
                       ; just ignore return/c for now
                       (if arg-ids
                           #`(func #,@arg-ids)
                           #`(apply func arglist)))])
         (quasisyntax/loc stx
           (lambda arglist
             (match arglist
               #,@(map (λ(spec) (spec->match-clause spec return))
                       (syntax->list #'(spec ...)))
               [else
                (raise-argument-error
                 'name
                 (format "An argument list satisfying one of the following:\n~a"
                         (format-specs '(spec ...)))
                 arglist)])))))]))

(module+ test
  (define (blah . args) args)

  (define blah1 (guard blah
                       [integer? integer? -> any]
                       [string? string? -> any]))
  )
