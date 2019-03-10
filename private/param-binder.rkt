#lang racket
(provide new-binder
         binder?
         encounter
         bound-values
         param-number)

; While rendering to SQL, we can record the occurence of params into a binder.
; Then the binder can be used to produce a list of parameter values in the
; correct order to send to the DB.

(require "core.rkt")

(struct binder ([params]) #:transparent)

(define (new-binder)
  (binder '()))

(define/contract (encounter b p)
  (-> binder? param? binder?)
  (define params (binder-params b))
  (if (member p params)
      b
      (struct-copy binder b
                   [params (cons p params)])))

; Returns parameter values in the correct order.
(define/contract (bound-values binder bindings)
  (-> binder? hash? list?)
  (define params (reverse (binder-params binder)))
  (define (get-value p)
    (hash-ref bindings p
              (λ()
                (define value (param-value p))
                (or value
                    (error "Parameter is unbound:: " p)))))
  (map get-value params))

(define/contract (param-number binder param)
  (-> binder? param? exact-positive-integer?)
  (add1 (or (index-of (reverse (binder-params binder)) param)
            (error "parameter was not encountered: " param))))
