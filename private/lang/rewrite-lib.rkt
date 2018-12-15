#lang racket

; This is kind of like a parser combinator except it doesn't parse.
; It rearranges syntax objects.
; Define "rewriter" to mean a procedure of (-> syntax? (or/c syntax? #f))
(define rewriter? procedure?)
; Now you can call this a "rewriter combinator" library.
; A rewriter returns #f to indicate that no rewrite took place.
; Combinators are named with a /slash

(provide /pass-ltr ; Walks the syntax object, including recursion
         /all      ; Run all rewriters in order
         /or       ; Run rewriters until one succeeds and return immediately
         /filter   ; Run rewriter if predicate succeeds, else return #f
         /log      ; Do some printlns if the rewriter succeeds
         /repeat   ; Run the rewriter repeatedly until it returns #f
         paren-shape-equal? braced?
         copy-props copy-srcloc copy-context)

(define/contract (copy-props orig-stx new-stx)
  (-> syntax? syntax? syntax?)
  (datum->syntax orig-stx
                 (syntax-e new-stx)
                 orig-stx orig-stx))

(define (copy-srcloc from to)
  (datum->syntax to (syntax-e to) from to))

(define (copy-context from to)
  (datum->syntax from (syntax-e to) to to))

(define/contract (/log rule)
  (-> rewriter? rewriter?)
  (λ(stx)
    (let ([result (rule stx)])
      (when result
        (print "from: ")
        (println stx)
        (print "to  : ")
        (println result))
      result)))

(define/contract (repeat stx rule on-exhausted)
  (-> syntax? rewriter? any/c any/c)
  (define accum #f)
  (define (loop stx)
    (let ([result (rule stx)])
      (if result
          (begin
            (set! accum result)
            (loop result))
          accum)))
  (loop stx))

(define/contract (/repeat rule)
  (-> rewriter? rewriter?)
  (λ(stx) (repeat stx rule #f)))

(define (run-all stx rules final-result)
  (match rules
    [(list rule1 rest ...)
     (begin
       (define result (rule1 stx))
       (run-all (or result stx)
                rest
                (or result final-result)))]
    [(list) final-result]))

(define/contract (/all . rules)
  (->* () #:rest (listof rewriter?) rewriter?)
  (λ(stx) (run-all stx rules #f)))

(define/contract (/pass-ltr rule)
  (-> rewriter? rewriter?)
  (λ(stx)
    (define any-success? #f)
    (define (looper stx)
      (define result (rule stx))
      (if result
          ; Success. Try again immdiately with the new stx.
          ; TODO or wouldn't it be better to have the caller pass in
          ; (/repeat rules) if that is what they want to happen? Hmm...
          (begin
            (set! any-success? #t)
            (looper result))
          ; Failure. Chop the head off the list and try again.
          (syntax-case stx ()
            [{a rest ...}
             (begin
               ; TODO a parameter or combinator should control if and when we recurse here
               (define A #'a)
               (set! A ((/pass-ltr rule) A))
               (when A (set! any-success? #t))
               (define REST (copy-props stx #'(rest ...)))
               (datum->syntax stx
                              (cons (or A #'a)
                                    (syntax->list (looper REST)))
                              stx stx))]
            [else stx])))
    (define result (looper stx))
    (if any-success? result #f)))

(define/contract (paren-shape-equal? shape stx)
  (-> (or/c #\( #\{ #\[) syntax? any/c)
  (equal? (syntax-property stx 'paren-shape) shape))

(define/contract (braced? stx)
  (-> syntax? any/c)
  (paren-shape-equal? #\{ stx))

(define/contract (/filter pred rule)
  (-> (-> syntax? any/c) rewriter? rewriter?)
  (λ(stx) (and (pred stx)
               (rule stx))))

(define/contract (/or . rules)
  (->* () #:rest (listof rewriter?) rewriter?)
  (λ(stx) (ormap (λ(rule) (rule stx)) rules)))

(module+ test
  (require rackunit)
  (define/contract (make-infix-rule symbols)
    (-> (listof symbol?) rewriter?)
    (λ(stx)
      (syntax-case stx ()
        [(a op b rest ...)
         (cond
           [(member (syntax-e #'op) symbols)
            #'((op a b) rest ...)]
           [else #f])]
        [else #f])))
  (define plus-minus (make-infix-rule '(+ -)))
  (define times-div (make-infix-rule '(* /)))

  (define (check rule stx expected)
    (check-equal? (syntax->datum (rule stx))
                  expected))
  (check (/pass-ltr plus-minus)
         #'{a + b - c}
         '{(- (+ a b) c)})
  (check (/all (/pass-ltr plus-minus))
         #'{a + b - c}
         '{(- (+ a b) c)})
  (check (/all
          (/pass-ltr times-div)
          (/pass-ltr plus-minus))
         #'{a + b * c - d / e}
         '{(- (+ a (* b c)) (/ d e))}))