#lang racket

(require (submod "./private2/sql.rkt" unsafe))
(provide (all-from-out (submod "./private2/sql.rkt" unsafe)))

(provide val)

(require "./private2/sql/fragment.rkt"
         "./private2/_types.rkt"
         "./private2/_null.rkt")

; TODO revisit this. Should support Bit and Datetime somehow.
(define/contract (val x)
  (-> (or/c number? string?) Scalar)
  (cond
    [(number? x)
     (>> (scalar x) #:cast Number #:null no)]
    [(string? x)
     ; TODO do \r and \n need to be escaped also?
     ; Also, we should probably do escaping during reduction
     (let* ([x (string-replace x "'" "''")]
            [x (format "'~a'" x)])
       (>> (scalar x) #:cast String #:null no))]
    [else
     (error "assert fail")]))
