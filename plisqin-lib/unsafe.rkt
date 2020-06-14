#lang racket

(require (submod "./private2/sql.rkt" unsafe))
(provide (all-from-out (submod "./private2/sql.rkt" unsafe)))

(provide val)

(require "./private2/sql/fragment.rkt"
         "./private2/_types.rkt"
         "./private2/_null.rkt")

(define/contract (val x [given-type #f])
  (->* [(or/c boolean? number? string?)]
       [(or/c #f type?)]
       Scalar?)
  (define type
    (or given-type
        (cond
          [(boolean? x) Bool?]
          [(number? x) Number?]
          [(string? x) String?]
          [else (error "assert fail")])))
  (define frag
    (cond
      [(boolean? x)
       (if x
           (sql "(1=1)")
           (sql "(1=0)"))]
      [(number? x)
       (scalar x)]
      [(string? x)
       (let* ([x (string-replace x "'" "''")]
              [x (format "'~a'" x)])
         (scalar x))]
      [else
       (error "assert fail")]))
  (>> frag #:cast type #:null no))
