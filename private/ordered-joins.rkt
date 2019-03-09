#lang racket
(provide ordered-joins)
(require "core.rkt" "util.rkt" "deduplicate.rkt")

; Returns the joins belonging to this query in the correct order to be rendered to SQL.
(define/contract (ordered-joins q)
  (-> query? (listof join?))
  (define (enter node)
    (cond
      [(or (query? node) (binding? node))
       (return (no-recurse node))]
      [else (return node)]))
  (define (exit node accum)
    (cond
      [(join? node)
       (return node (cons node accum))]
      [else (return node)]))
  (define (find x) (walk x
                         #:on-enter enter
                         #:on-exit exit
                         #:accum '()
                         #:return 'accum
                         #:skip-root #t))
  (remove-duplicates (append (reverse (find (s:query-joins q)))
                             (reverse (find (query-fragments q))))))

(module+ test
  (require rackunit "macros.rkt"))
(define-syntax-rule (test FORMS ...)
  (module+ test
    (let ([a 0])
      FORMS
      ...)))

(test
 ; Two locally-bound joins - just take them in the order they appear in code
 (define q
   (from a "A"
         (join x "X")
         (join y "Y")))
 (check-equal?
  (map get-alias (ordered-joins q))
  (list "x" "y")))
(test
 ; The two locally-bound joins (x and y) come first. Then the inline join (z).
 (define q
   (from a "A"
         (join x "X")
         (select (join z "Z"))
         (join y "Y")))
 (check-equal?
  (map get-alias (ordered-joins q))
  (list "x" "y" "z")))
(test
 ; The joins nested within join-on clauses must come first.
 ; For example, y depends on y2, so y2 must come before y.
 ; As usual, x beats y because it is locally-bound and y is inline.
 (define q
   (from a "A"
         (select (join y "Y"
                       (join-on  (join y2 "Y2"))))
         (join x "X"
               (join-on (join x2 "X2")))))
 (check-equal?
  (map get-alias (ordered-joins q))
  (list "x2" "x" "y2" "y")))
(test
 (define q
   (deduplicate
    (from x "X"
          ; This is an inline join which should be returned
          (RS select (join y "Y")".something"))))
 (check-equal?
  (ordered-joins q)
  (list (deduplicate (join y "Y")))))
(test
 ; Here we make y a binding instead of an inline join.
 ; When we find it in a query's fragments, we don't consider it
 ; a part of that query's joins. (In actual usage, the non-binding
 ; version should be found in a "query-joins" list, either on the
 ; current query or some enclosing query.)
 (define y (binding (join y "Y")))
 (define q
   (deduplicate
    (from x "X"
          (RS select y".something"))))
 (check-equal? (ordered-joins q) '()))
