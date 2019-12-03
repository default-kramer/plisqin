#lang racket

; Make sure we are providing the same set of ids for each variation
(define-syntax-rule (do-provide id)
  (module+ id
    (require (submod "./sql/frags.rkt" id))

    (provide join-on select where group-by having order-by
             scalar aggregate bool subquery sql
             count avg min max sum exists)

    (module+ operators
      (require (submod "./sql/frags.rkt" id operators))

      (provide and or not
               = <> < <= > >=
               like not-like
               is is-not
               in not-in
               + - * /))))

(do-provide untyped)
;(do-provide loose)
;(do-provide strict)

(module+ test
  (require rackunit
           (submod ".." untyped)
           (submod ".." untyped operators))

  ; Check that each id is bound to a fragment constructor.
  (define-syntax-rule (check-frags frag-id ...)
    (begin
      ; The untyped fragment constructor should accept any arguments
      (let* ([result (frag-id 1 "2" '(three four))]
             [expected (format "(~a 1 \"2\" '(three four))" 'frag-id)]
             [actual (format "~v" result)])
        (check-equal? expected actual))
      ...))

  (check-frags join-on select where group-by having order-by
               scalar aggregate bool subquery sql
               count avg min max sum exists)
  (check-frags and or not
               = <> < <= > >=
               like not-like
               is is-not
               in not-in
               + - * /))
