#lang racket

(provide make tokens parens insert-ands interpose ->Bit ->Bool
         (for-syntax TODO matchup))

(require "fragment.rkt"
         "../_types.rkt"
         "../_dialect.rkt"
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

; `make` is used to construct fragments
(define-syntax (make stx)
  (syntax-case stx ()
    [(_ id kind content)
     (syntax/loc stx
       (make id kind content #:reduce-proc identity))]
    [(_ id kind content #:reduce reduce-body)
     (syntax/loc stx
       (make id kind content
             #:reduce-proc
             (λ (tokes)
               (syntax-parameterize ([tokens (TODO #'tokes)])
                 reduce-body))))]
    [(_ :id :kind :content #:reduce-proc :reducer)
     (syntax/loc stx
       (new fragment%
            [id :id]
            [kind :kind]
            [content :content]
            [type #f]
            [as-name #f]
            [reducer :reducer]))]))

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


; To be used during reduction.
; Converts a Bool to a Bit (only needed for MSSQL)
(define (->Bit tokens)
  (match tokens
    [(list a)
     #:when (and (Bool? a)
                 (mssql? (current-dialect)))
     (list "cast(case when "a" then 1 else 0 end as bit)")]
    [else tokens]))

; To be used during reduction.
; Converts a Bit to a Bool (needed by all dialects)
(define (->Bool tokens)
  (match tokens
    [(list a)
     #:when (Bit? a)
     (list a" <> 0")]
    [else tokens]))


; helper that flattens
#;([(a b) foo]
   [(c d) bar])
; into a list like [a foo] [b foo] [c bar] [d bar]
(define-for-syntax (matchup stx)
  (syntax-case stx ()
    [([(id rest ...) body]
      more ...)
     (cons
      (syntax/loc #'id
        [id body])
      (matchup #'([(rest ...) body]
                  more ...)))]
    [([() body]
      more ...)
     (matchup #'(more ...))]
    [()
     (list)]
    [else (error "assert fail t42j90b")]))
