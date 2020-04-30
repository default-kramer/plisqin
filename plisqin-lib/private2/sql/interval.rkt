#lang racket

(provide interval? make-interval interval-unit interval-qty)

; An interval unit is one of '(year month day hour minute second).
; The interval qty is usually a number? but it can be any token.

(require "../_null.rkt")

(define (normalize unit)
  (case unit
    [(years year) 'year]
    [(months month) 'month]
    [(days day) 'day]
    [(hours hour) 'hour]
    [(minutes minute) 'minute]
    [(seconds second) 'second]
    [else #f]))

(define (make-interval unit qty)
  (let ([unit (normalize unit)])
    (if unit
        (new interval% [unit unit] [qty qty])
        (error "contract violation, not a unit:" unit))))

(define interval%
  (class* object% (nulltrack<%>)
    (init-field unit qty)
    (super-new)

    ; nulltrack<%>
    (define/public (get-fallback) #f)
    (define/public (get-nullability)
      (nullability qty))
    ))

(define interval? (is-a?/c interval%))
(define interval-unit (class-field-accessor interval% unit))
(define interval-qty (class-field-accessor interval% qty))
