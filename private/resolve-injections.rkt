#lang racket
(provide resolve-injections)

(require "core.rkt" "util.rkt" "deduplicate.rkt"
         (prefix-in racket: racket))
(module+ test
  (require rackunit "macros.rkt"))

; There are two types of injections that are different enough that we handle them in two distinct passes.
; Recall that an injection is (target, placeholder, fragment) where fragment gets injected into target
; after replacing placeholder with the source of the target.
; If the target is a source, call it a "local injection".
; Otherwise the target must be a binding or an inline join; call it a "nonlocal injection".
; The most common occurence of a local injection is (join-on (inject me "FOO") ...)
; Nonlocal injections are commonly aggregates, such as (inject [x (Grouped-join a)] "sum(x.BAR)")
(define (local-injection? x)
  (and (injection? x)
       (not (source? (s:injection-target x)))))
(define (nonlocal-injection? x)
  (and (injection? x)
       (not (local-injection? x))))

; The overall algorithm will work one injection at a time, to greatly simplify
; (but not eliminate) the problem of trying to replace an element that no longer exists
; because other replacements have already occurred below it.
; First we will resolve local injections throughout every join in the query.
; Then we will resolve nonlocal injections throughout the query.
(define/contract (resolve-injections q)
  (-> query? query?)
  ; Resolve local injections for all joins
  (define (resolve-local node)
    (cond
      [(join? node) (return (resolve-local-injections node))]
      [else (return node)]))
  (set! q (walk q #:on-enter resolve-local))
  ; Now resolve nonlocal injections
  (define (resolve q)
    (define new-q (step-nonlocal q))
    (if new-q
        (resolve new-q)
        q))
  (resolve q))

; To resolve local injections, we are given a join J and we:
; 1) find a local injection {one whose target is (get-src J)}
; 2) add a select clause to J, e.g. (select "whatever as __INJECT94")
; 3) replace the injection with e.g. (scalar (get-src J)".__INJECT94")
; Repeat until step 1 fails to find an injection.
(define/contract (resolve-local-injections j)
  (-> join? join?)
  ; accum is #f until we find the first injection
  (define (find-injection node accum)
    (cond
      [accum (return (no-recurse node))]
      [(and (injection? node)
            (equal? (get-src j) (s:injection-target node)))
       (return (no-recurse node)
               node)]
      [else (return node)]))
  (define inj (walk j #:on-enter find-injection #:accum #f #:return 'accum))
  ; If inj was found we will do this
  (define (replace-and-recurse)
    (define colname (get-colname j))
    (define the-scalar (scalar (get-src j) (raw-sql "."colname)))
    (set! j (add-select-clause j inj colname))
    (set! j (replace j inj the-scalar))
    (resolve-local-injections j))
  (if inj
      (replace-and-recurse)
      j))

; To resolve nonlocal injections, we are given a query Q and we:
; 1) Find a nonlocal injection I, its target T, and the join J to which it refers.
;       Note: methinks T will always be J or (binding J) but I'm not sure...
; 2) Create J' by adding the select clause to J.
;   2a) Create T' which is (replace T J J')
;   2b) Create I' which is (replace I J J')
; 3) Create S'' which is (scalar T' "." colname)
;   3a) Create J'' which is (replace J' I S'')
; 4) Return Q'' which is (replace Q [I S''] [J J''])
(define/contract (step-nonlocal q)
  (-> query? (or/c #f query?))
  (define temp (find-injection q))
  (define (GO)
    ; The order of the replacements here is really confusing :(
    (define inj (car temp))
    (define join (cdr temp))
    (define target (s:injection-target inj))
    (define ORIG-inj inj)
    (define ORIG-join join)
    (define select-as (get-colname join))
    ; Update join, don't update query yet
    (set! join (add-select-clause join inj select-as))
    (set! target (replace target ORIG-join join))
    (set! inj (replace inj ORIG-join join))
    ; Replace injection with scalar in EVERYTHING
    (define the-scalar (scalar target (raw-sql "."select-as)))
    (set! q (replace q ORIG-inj the-scalar))
    (set! join (replace join ORIG-inj the-scalar))
    (set! q (replace q ORIG-join join))
    q)
  (if temp (GO) #f))

(test
 (define j
   (deduplicate
    (RS join j "J"
        (join-on (inject j j".foo")" = 1")
        (join-on (inject j j".foo")" = 2")
        (join-on (inject j j".bar")" = 3")
        (join-on (inject j j".bar")" = 4"))))
 (check-equal?
  (normalize (resolve-local-injections j))
  (normalize (deduplicate
              (RS join j "J"
                  (select (scalar j".foo")" as __INJECT1")
                  (select (scalar j".bar")" as __INJECT2")
                  (join-on (scalar j".__INJECT1")" = 1")
                  (join-on (scalar j".__INJECT1")" = 2")
                  (join-on (scalar j".__INJECT2")" = 3")
                  (join-on (scalar j".__INJECT2")" = 4"))))))

(define/contract (find-join inj ancestry)
  (-> injection? list? join?)
  (let ([target (s:injection-target inj)])
    (cond
      [(join? target) target]
      [(binding? target) (s:binding-join target)]
      [(source? target)
       (first (filter (λ(j) (and (join? j)
                                 (equal? (get-src j) target)))
                      ancestry))])))

(define/contract (find-injection root)
  (-> any/c (or/c #f (cons/c injection? join?)))
  (define (visit node accum ancestry)
    (cond
      [accum (return (no-recurse node))]
      [(injection? node)
       (return node
               (cons node (find-join node ancestry)))]
      [else (return node)]))
  (walk root #:on-enter visit #:accum #f #:return 'accum))

(define/contract (add-select-clause j i select-as)
  (-> join? injection? raw-sql? join?)
  ; Adds a select clause to the join for the given injection
  (define/contract (remove-placeholder)
    (-> fragment?)
    (define (visit node)
      (cond
        [(equal? node (s:injection-placeholder i))
         ; The fragment we are returning is going to to be placed in the join's
         ; select clause. So we know (get-src j) is in scope, and is the correct
         ; way to refer to the join from within itself.
         (return (get-src j))]
        [else (return node)]))
    (walk (s:injection-fragment i) #:on-enter visit))
  (add-statement j (select (remove-placeholder) (raw-sql" as "select-as))))

(define/contract (get-colname j)
  (-> join? raw-sql?)
  ; Returns a column name like "__INJECT%" to be used for the injection.
  ; For example, if the join already has 2 select clauses, the column name will be "__INJECT3"
  (let* ([q (s:join-query j)]
         [num-selects (length (query-fragments q 'Select))])
    (raw-sql (format "__INJECT~a" (add1 num-selects)))))

(test
 (define q
   (deduplicate
    (RS from x "X"
        (join y "Y"
              (group-by y".BLAH")
              (join-on (inject y y".F1")))
        (select (inject y "sum("y".Stuff)"))
        (select (inject y "sum("y".Stuff)")))))
 (define expected
   (deduplicate
    (RS from x "X"
        (join y "Y"
              (group-by y".BLAH")
              (join-on (scalar y".__INJECT1"))
              (select (scalar y".F1")" as __INJECT1")
              (select (scalar "sum("y".Stuff)")" as __INJECT2"))
        (select (scalar y".__INJECT2"))
        (select (scalar y".__INJECT2")))))
 (check-equal?
  (normalize (resolve-injections q))
  (normalize expected)))

(test
 ; Tests nested injections. Main query is Employee.
 ; Each Employee has a group of Checkouts.
 ; Each Checkout has a group of Rentals.
 ; This is simulating the injections that would result from
 (void '(sum (sum (Rentals-of/g (Checkouts-of/g employee)))))

 (define (Rentals-of/g checkout)
   (RS join r "Rental"
       (group-by r".CheckoutId")
       (join-on r".CheckoutId = "checkout".CheckoutId")))
 (define (Checkouts-of/g employee)
   (RS join c "Checkout"
       (group-by c".EmployeeId")
       (join-on c".EmployeeId = "employee".EmployeeId")))
 (define q
   (deduplicate
    (RS from e "Employee"
        (select (inject [c (Checkouts-of/g e)]
                        "sum("
                        (inject [r (Rentals-of/g c)]
                                "sum("
                                (scalar r".Cost")
                                ")")
                        ")")
                " as TotalCost"))))
 (define expected
   (deduplicate
    (RS from e "Employee"
        (define (RENTALS c)
          (join r (Rentals-of/g c)
                (select (scalar "sum("
                                (scalar r".Cost")
                                ")")
                        " as __INJECT1")))
        (define (CHECKOUTS e)
          (join c (Checkouts-of/g e)
                (select (scalar "sum("
                                (scalar (RENTALS c)".__INJECT1")
                                ")")
                        " as __INJECT1")))
        (select (scalar (CHECKOUTS e)".__INJECT1")" as TotalCost"))))
 (check-equal?
  (normalize (resolve-injections q))
  (normalize expected)))
