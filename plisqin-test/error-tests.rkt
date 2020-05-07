#lang racket

(require plisqin
         rackunit)

(define-syntax (check-exn+message stx)
  (syntax-case stx ()
    [(_ expr snippet)
     (syntax/loc stx
       (check-exn+message expr snippet exn?))]
    [(_ expr snippet exn-contract?)
     #`(begin
         #,(syntax/loc stx
             (check-exn exn-contract? (lambda () expr)))
         (with-handlers ([exn? (lambda (exn)
                                 (let ([message (exn-message exn)])
                                   #,(syntax/loc stx
                                       (check-true (string-contains? message snippet)
                                                   (format "Missing: ~a   ***   From: ~a"
                                                           snippet message)))))])
           expr))]))

(check-exn+message (join-on "not a Token")
                   "join-on: contract violation")
(check-exn+message (coalesce "not a Token")
                   "coalesce: contract violation")

(define/contract (test-that-instanceof-prints-correctly x)
  (-> (instanceof 'Foo) any/c)
  x)
(check-exn+message (test-that-instanceof-prints-correctly 42)
                   "(instanceof 'Foo)")
