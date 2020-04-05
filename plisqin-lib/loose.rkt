#lang racket

(require (submod "./private2/sql.rkt" loose))
(provide (all-from-out (submod "./private2/sql.rkt" loose)))

(provide val)

(require (prefix-in %% "unsafe.rkt"))

(define-syntax (val stx)
  (syntax-case stx ()
    [(name x)
     (let ([c (syntax-e #'x)])
       (cond
         [(or (string? c)
              (number? c))
          (syntax/loc stx
            (%%val x))]
         [else
          (syntax/loc stx
            (raise-argument-error 'name "a literal (string or numeric)" x))]))]))
