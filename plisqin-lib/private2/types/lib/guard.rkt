#lang racket

(provide guard func-name return-handler)

; Consider
#;(guard foo
         [integer? integer? -> integer?]
         [string? ...+ -> list?])
; This says that `foo` will accept two integers and return an integer,
; or it will accept one or more strings and return a list.
; Any other argument list is an error. Keyword arguments are not supported.
; NOTE - the return contract is not enforced by default; you may
; syntax-parameterize the `return-handler` to handle this.

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

  ;;; spec->match-clause
  ; Given a spec like [foo? bar? -> baz?] expand to something like
  #;[(list a b)
     #:when (and (foo? a)
                 (bar? b))
     (let ([retval (func a b)])
       (handle-return-type retval baz?))]
  ; If the spec has ... or ...+ in it, we will need to bind retval to
  #;(apply func arglist)
  ; instead, but `(func a b)` is faster if we have already matched all the args.
  (define (spec->match-clause spec func arglist)
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
              (let ([retval #,(if has-rest?
                                  #`(apply #,func #,arglist)
                                  #`(#,func arg-id ...))])
                (return-handler retval s.return/c))])))])))

(define (format-specs specs)
  (string-join (map ~a specs) "\n"))

(define-syntax-parameter func-name (λ (stx) #'#f))

(define-syntax-parameter return-handler
  (λ (stx) (syntax-case stx ()
             [(_ retval contract)
              ; just assume that it satisfies the contract
              #'retval])))

(define (display-types arglist)
  (map (λ (x) (or (get-type x) 'Untyped))
       arglist))

(define-syntax (guard stx)
  (syntax-parse stx
    [(_ func:id spec ...+)
     (with-syntax ([arglist #'arglist])
       (quasisyntax/loc stx
         (lambda arglist
           (match arglist
             #,@(map (λ(spec) (spec->match-clause spec #'func #'arglist))
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
               (display-types arglist))]))))]))

(module+ test
  (define (blah . args) args)

  (define blah1 (guard blah
                       [integer? integer? -> any]
                       [string? string? -> any])))
