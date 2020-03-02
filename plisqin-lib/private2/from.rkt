#lang racket

(provide from join)

(require (prefix-in m: (submod "_core.rkt" from-helper)))

(define-for-syntax (tweak stx)
  (syntax-case stx (join)
    [(join a b clause ...)
     (quasisyntax/loc stx
       (m:attach a b #,@(map tweak (syntax->list #'(clause ...)))))]
    [_ stx]))

(define-syntax-rule (wrap expr)
  (parameterize ([m:flatten-lists? #t])
    expr))

(define-syntax (from stx)
  (syntax-case stx ()
    [(_ a b clause ...)
     (quasisyntax/loc stx
       (wrap (m:from a b #,@(map tweak (syntax->list #'(clause ...))))))]))

(define-syntax (join stx)
  (syntax-case stx ()
    [(_ a b #:to c clause ...)
     (quasisyntax/loc stx
       (wrap (m:join a b #:to c #,@(map tweak (syntax->list #'(clause ...))))))]
    ; TODO we want #:to to be required, but leave it optional for now
    [(_ a b clause ...)
     (quasisyntax/loc stx
       (wrap (m:join a b #,@(map tweak (syntax->list #'(clause ...))))))]))

(module+ test
  (require rackunit)

  (check-equal? (from a "A"
                      (join b "B"
                            '(flatten this list))
                      '(flatten this too))
                (from a "A"
                      (join b "B"
                            'flatten 'this 'list)
                      'flatten 'this 'too)))
