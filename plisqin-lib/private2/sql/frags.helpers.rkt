#lang racket

(provide def tokens parens insert-ands interpose export ->Bit ->Bool)

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

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ :id #:kind :kind #:reduce :reduction)
     (syntax/loc stx
       (define (:id . content)
         (new fragment%
              [id ':id]
              [kind :kind]
              [content content]
              [type #f]
              [as-name #f]
              [reducer
               (λ (content)
                 (syntax-parameterize ([tokens (TODO #'content)])
                   :reduction))])))]))


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


(define-for-syntax (unintern-id id)
  (datum->syntax id
                 (string->uninterned-symbol (format "~a" (syntax-e id)))))

(define-syntax (export-single stx)
  (syntax-case stx ()
    [(_ keyword raw-id)
     (let ([table (syntax-case #'keyword ()
                    [#:unsafe :unsafe-table]
                    [#:loose :loose-table]
                    [#:strict :strict-table])])
       (with-syntax ([id (unintern-id #'raw-id)])
         (quasisyntax/loc stx
           (begin
             (provide (rename-out [id raw-id]))
             (define id #,(table #'raw-id))))))]))

(define-syntax-rule (export keyword id ...)
  (begin (export-single keyword id) ...))


; To be used during reduction.
; Converts a Bool to a Bit (only needed for MSSQL)
(define (->Bit tokens)
  (match tokens
    [(list a)
     #:when (and (Bool a)
                 (mssql? (current-dialect)))
     (list "cast(case when "a" then 1 else 0 end as bit)")]
    [else tokens]))

; To be used during reduction.
; Converts a Bit to a Bool (needed by all dialects)
(define (->Bool tokens)
  (match tokens
    [(list a)
     #:when (Bit a)
     (list a" = 1")]
    [else tokens]))
