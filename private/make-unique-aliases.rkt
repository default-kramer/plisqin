#lang racket
(provide make-unique-aliases)
(require "core.rkt" "util.rkt" "ordered-joins.rkt" "deduplicate.rkt"
         (prefix-in racket: racket))

(define (get-alias x)
  (s:source-alias (get-src x)))

(define (is-subquery? ancestry)
  (and ((length ancestry) . >= . 2)
       (query? (first ancestry))
       (not (join? (second ancestry)))))

(define/contract (make-unique-aliases q)
  (-> query? query?)
  (ONETIME '() q))

(define/contract (re-alias j counter used-aliases)
  (-> (or/c join? query?) integer? (listof string?) (or/c join? query?))
  (define alias (format "~a~a" (get-alias j) counter))
  (if (member alias used-aliases)
      (re-alias j (add1 counter) used-aliases)
      (replace j
               (get-src j)
               (struct-copy s:source (get-src j)
                            [alias alias]))))

; This is going to be much easier if we do one at a time, right?
(define/contract (ONETIME external-aliases q)
  (-> (listof string?) query? query?)
  (define (main-body)
    (define (do-subqueries node aliases ancestry)
      (cond
        [(is-subquery? ancestry)
         (return (no-recurse (ONETIME aliases node)))]
        [else (return node)]))
    (define joins (ordered-joins q))
    (define reserved-aliases (cons (get-alias q) external-aliases))
    (define groups (racket:group-by get-alias joins))
    (define/contract (find-conflict group-of-joins)
      (-> (listof join?) (or/c join? #f))
      (match group-of-joins
        [(list) (error "assert false")]
        [(list first rest ...)
         #:when (member (get-alias first) reserved-aliases)
         first]
        [(list first next rest ...) next]
        [else #f]))
    ; (filter identity ...) will remove groups that didn't have a conflict and returned #f
    (define conflicts (filter identity (map find-conflict groups)))
    (define all-aliases (append reserved-aliases (map get-alias (flatten groups))))
    (if (empty? conflicts)
        ; go deeper
        (walk q
              #:on-enter do-subqueries
              #:accum all-aliases
              #:return 'root
              #:skip-root #t)
        ; replace conflict
        (let* ([conflict (first conflicts)]
               [unconflict (re-alias conflict 1 all-aliases)]
               [q (replace q conflict unconflict)])
          (ONETIME external-aliases q))))
  ; First, if this query's alias is already in use, reassign it
  (if (member (get-alias q) external-aliases)
      (re-alias q 1 external-aliases)
      (main-body)))

(module+ test
  (require rackunit "macros.rkt"))
(define-syntax-rule (test FORMS ...)
  (module+ test
    (let ([ignored 0])
      FORMS
      ...)))

(test
 (define q
   (from x "Parent"
         (where (exists (from x "Sub")))))
 (define expected
   (from x "Parent"
         (where (exists (from x1 "Sub")))))
 (check-equal?
  (make-unique-aliases q)
  expected))

(test
 (define q
   (from x "X"
         (join y "A")
         (join y "B")
         (join y "C")))
 (define expected
   (from x "X"
         (join y "A")
         (join y1 "B")
         (join y2 "C")))
 (check-equal?
  (make-unique-aliases q)
  expected))

(test
 ; The outer alias will be "s" and the inner alias is a parameter
 (define/contract (make-query inner-alias)
   (-> string? query?)
   (from e "Employee"
         (join s (source "s" "Store")
               (join-on s e))
         (where (exists (from a "A"
                              (join s2 (source inner-alias "NotStore"))
                              (select s s2))))))
 ; Here the inner alias "s" gets rewritten to "s1"
 (check-equal?
  (make-unique-aliases (make-query "s"))
  (make-query "s1"))
 ; Here the inner alias is unique so nothing is rewritten
 (check-equal?
  (make-unique-aliases (make-query "unique"))
  (make-query "unique")))

(test
 ; The outermost "x" will conflict with two "x"s within the subquery.
 ; This tests which order they get reassigned.
 (define q
   (from x "X"
         (where (exists (from y "Y"
                              (join x "A")
                              (join x "B"))))))
 (define expected
   (from x "X"
         (where (exists (from y "Y"
                              (join x1 "A")
                              (join x2 "B"))))))
 (check-equal?
  (make-unique-aliases q)
  expected))

(test
 ; The deduplication means that the bound join and the inline join
 ; are the same thing. Therefore no changes to aliases are needed.
 (define q
   (deduplicate
    (from a "A"
          (join y "Y")
          (select y".bound")
          (select (join y "Y")".inline"))))
 (check-equal?
  q
  (make-unique-aliases q)))