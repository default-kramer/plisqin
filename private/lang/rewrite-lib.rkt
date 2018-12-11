#lang racket

(provide apply-rules paren-shape-equal? braced?
         /or /filter)

; rewriter? means a procedure of (-> syntax? (or/c syntax? #f))
(define rewriter? procedure?)

(define/contract (copy-props orig-stx new-stx)
  (-> syntax? syntax? syntax?)
  (datum->syntax orig-stx
                 (syntax-e new-stx)
                 orig-stx orig-stx))

(define/contract (apply-rules rules stx)
  (-> rewriter? syntax? syntax?)
  (let ([result (rules stx)])
    (match result
      [s #:when (syntax? s)
         (apply-rules rules result)]
      [#f (match (syntax->list stx)
            [(list a rest ...)
             (begin
               (define a1 (apply-rules rules a))
               (define rest1 (apply-rules rules (copy-props stx #`(#,@rest))))
               (copy-props stx #`(#,a1 . #,rest1)))]
            [else stx])]
      [else stx])))

(define/contract (paren-shape-equal? shape stx)
  (-> (or/c #\( #\{ #\[) syntax? any/c)
  (equal? (syntax-property stx 'paren-shape) shape))

(define/contract (braced? stx)
  (-> syntax? any/c)
  (paren-shape-equal? #\{ stx))

(define/contract (/filter pred rules)
  (-> (-> syntax? any/c) rewriter? rewriter?)
  (λ(stx) (and (pred stx)
               (rules stx))))

(define/contract (/or . rules)
  (->* () #:rest (listof rewriter?) rewriter?)
  (λ(stx) (ormap (λ(rule) (rule stx)) rules)))