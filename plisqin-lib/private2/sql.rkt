#lang racket

(define-syntax-rule (do-provide id)
  (module+ id
    (require (submod "./sql/frags.rkt" id))
    (provide (all-from-out (submod "./sql/frags.rkt" id)))

    (module+ operators
      (require (submod "./sql/frags.rkt" id operators))
      (provide (all-from-out (submod "./sql/frags.rkt" id operators))))))

(do-provide unsafe)
(do-provide loose)
(do-provide strict)
