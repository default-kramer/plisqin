#lang racket

(require (only-in "_null.rkt" fallback?)
         (only-in "./sql/fragment.rkt" >>))

(define-syntax-rule (do-provide id ooo)
  (module+ id
    (require (submod "./sql/frags.rkt" id))
    (provide (all-from-out (submod "./sql/frags.rkt" id)))

    (provide ??)
    (define (?? . tokens)
      (match tokens
        [(list a b)
         #:when (fallback? b)
         (>> a #:fallback b)]
        [(list a ooo b)
         #:when (fallback? b)
         (>> (apply coalesce a) #:fallback b)]
        [else
         (apply coalesce tokens)]))

    (module+ operators
      (require (submod "./sql/frags.rkt" id operators))
      (provide (all-from-out (submod "./sql/frags.rkt" id operators))))))

(do-provide unsafe ...)
(do-provide loose ...)
(do-provide strict ...)
