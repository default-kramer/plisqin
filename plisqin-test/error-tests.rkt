#lang racket

(require plisqin
         rackunit)

(define-syntax (check-exn+message stx)
  (syntax-case stx ()
    [(_ expr message-regexp)
     (syntax/loc stx
       (check-exn+message expr message-regexp exn?))]
    [(_ expr message-regexp exn-contract?)
     #`(begin
         #,(syntax/loc stx
             (check-exn exn-contract? (lambda () expr)))
         (with-handlers ([exn? (lambda (exn)
                                 #,(syntax/loc stx
                                     (check-regexp-match message-regexp (exn-message exn))))])
           expr))]))

(check-exn+message (join-on "not a Token")
                   "join-on: contract violation")
(check-exn+message (coalesce "not a Token")
                   "coalesce: contract violation")