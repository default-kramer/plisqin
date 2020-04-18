#lang racket

(provide func-name build-typechecker)
; This file should probably be renamed, because `guard` no longer exists.

(require (for-syntax syntax/parse)
         racket/stxparam
         (only-in "core.rkt" get-type))

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

  ;;; spec->match-clause2
  ; Given a spec like [foo? bar? -> Baz] expand to something like
  #;[(list a b)
     #:when (and (foo? a)
                 (bar? b))
     Baz]
  ; Also handles ...+ patterns like [foo? ...+ -> Bar] which expands like
  #;[(list a rest ...)
     #:when (and (foo? a)
                 (andmap foo? rest)
                 ; ensure list not empty because ...+ means "at least one more"
                 (pair? rest))
     Bar]
  (define (spec->match-clause2 spec)
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
              s.return/c])))])))

(define (format-specs specs)
  (string-join (map ~a specs) "\n"))

(define-syntax-parameter func-name (λ (stx) #'#f))

(define (display-types arglist)
  (map (λ (x) (or (get-type x) 'Untyped))
       arglist))

;;; build-typechecker
; (build-typechecker arglist spec ...) constructs an expression that
; checks (at runtime) the arglist to see if it satisfies any of the specs.
; If so, the expression's value will be the return type.
; Otherwise an type error will be raised.
; As a rough idea [Number Number -> Number] expands to something like
#;(match arglist
    [(list a b) #:when (and (Number a)
                            (Number b))
                Number]
    [else
     (error "Expected two numbers")])
(define-syntax (build-typechecker stx)
  (syntax-parse stx
    [(_ arglist0 spec ...+)
     (quasisyntax/loc stx
       (let ([arglist arglist0])
         (match arglist
           #,@(map (λ(spec) (spec->match-clause2 spec))
                   (syntax->list #'(spec ...)))
           [else
            (raise-argument-error
             (or (func-name) 'func)
             (format "An argument list satisfying one of the following:\n~a"
                     (format-specs '(spec ...)))
             ; It might be possible to print the actual values in addition
             ; to the types, but be careful. A naive strategy will trigger the
             ; infinite cycle detector. You would have to add a parameter or
             ; something that says "don't raise an error on infinite cycles,
             ; just print #whatever instead."
             ; But for now, let's just display the types.
             (display-types arglist))])))]))

(module+ test
  (require rackunit)

  (define (f . arglist)
    (build-typechecker arglist
                       [number? -> "One Number"]
                       [number? number? -> "Two Numbers"]
                       [number? number? ...+ -> "Many Numbers"]
                       [number? string? -> "Number, String"]
                       [string? -> "One String"]))

  (check-equal? (f 1)
                "One Number")
  (check-equal? (f 1 2)
                "Two Numbers")
  (check-equal? (f 1 2 3 4 5)
                "Many Numbers")
  (check-equal? (f 1 "hi")
                "Number, String")
  (check-equal? (f "hi")
                "One String"))
