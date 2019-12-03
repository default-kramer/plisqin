#lang racket

(provide def tokens parens insert-ands interpose)

(require "fragment.rkt"
         racket/stxparam)

(module+ test
  (require rackunit))

; TODO maybe tighten this up and add it to Morsel
(define (parens x)
  (match x
    [(list "(" a ")")
     x]
    [else (list "(" x ")")]))

(module+ test
  (check-equal? (parens (parens "foo"))
                (parens "foo")))


; TODO this must be built in somewhere
(define-for-syntax (TODO id-stx)
  (lambda (x)
    (syntax-case x ()
      [(f stuff ...)
       (quasisyntax/loc x
         (#%app #,id-stx stuff ...))]
      [id
       (quasisyntax/loc x #,id-stx)])))

(define-syntax-parameter tokens #f)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ :id #:kind :kind #:reduce :reduction)
     (syntax/loc stx
       (define (:id . content)
         (new fragment%
              [id ':id]
              [kind :kind]
              [content content]
              [reduction (syntax-parameterize ([tokens (TODO #'content)])
                           :reduction)])))]))


;;; insert-ands
; If the list of tokens is more than 3, insert " and " to join them.
(define (insert-ands id tokens)
  (match tokens
    [(list a b)
     (list a (format " ~a " id) b)]
    [(list a b rest ...)
     (list (insert-ands id (list a b))
           " and "
           (insert-ands id (cons b rest)))]))

(module+ test
  (check-equal?
   (string-join (flatten (insert-ands '<> '("foo" "bar" "baz"))) "")
   "foo <> bar and bar <> baz"))


(define (interpose x lst)
  (match lst
    [(list a b rest ...)
     (cons a (cons x (interpose x (cons b rest))))]
    [(list a)
     (list a)]
    [(list)
     (list)]))

(module+ test
  (check-equal?
   (interpose '+ '(1 2 3 4))
   '(1 + 2 + 3 + 4)))
