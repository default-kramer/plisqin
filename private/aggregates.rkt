#lang racket
(require "core.rkt" "aggregate.rkt")
(provide count sum min max avg)

(define-syntax-rule (def-agg name single-id-handler)
  (define/contract (name #:distinct? [distinct? #f] . tokens)
    (->* () (#:distinct? any/c) #:rest token-list? aggregate?)
    (define distinct-str (if distinct?
                             "distinct "
                             ""))
    (define name-str (~a 'name))
    (match tokens
      [(list x)
       #:when (or (source? x)
                  (join? x)
                  (binding? x))
       (if single-id-handler
           (single-id-handler x)
           (raise-argument-error 'tokens "a scalar-looking SQL expression" x))]
      [else (RS aggregate (raw-sql name-str) "(" (raw-sql distinct-str) tokens ")")])))
(def-agg count (λ(x) (RS aggregate "count(*)" (silence x))))
(def-agg sum #f)
(def-agg min #f)
(def-agg max #f)
(def-agg avg #f)