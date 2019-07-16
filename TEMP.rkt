#lang racket

(require plisqin
         ; TODO
         plisqin-lib/private/lang/default-require
         (only-in plisqin-lib/private/to-sql
                  bind-sql)
         (only-in plisqin-lib/private/param-binder
                  bound-values))

(require (for-syntax plisqin-examples/sakila)
         plisqin-examples/sakila)
(require (prefix-in db: db))

(define-schema connect-sakila)

(define conn (connect-sakila))
(current-connection conn)
(current-dialect 'sqlite) ; TODO not sure why (current-connection conn) isn't enough


; TODO what is the ideal way to interface with db?
; def-statement and make-statement seem good to me, but they shouldn't be
; composable, right? (In other words, they are "top-level".)
; But we should be able to automatically execute queries, I think.
; (And then why not all tokens?)
; For now, let's just make (go x)
(define (go x)
  (define-values (sql binder)
    (match (bind-sql x)
      [(cons a b) (values a b)]))
  (define param-values (bound-values binder (hash)))
  ; TODO ideally show-table would contain the binding logic we have just done
  (if (empty? param-values)
      (show-table x)
      (apply db:query-rows (list* conn sql param-values))))


; Imagine we are building a back-office system for the sakila database.
; Task 1 - a query to support a live feed of rentals.
(go (from R rental
          (order-by 'desc (rental_id R))
          (limit 10)))

(define-syntax-rule (singular-join [a b] col ...)
  (join a (a)
        (join-on (= (col a)
                    (col b)))
        ...))

(def/append! (inventory R)
  [(rental? R)
   (singular-join [inventory R] inventory_id)])

(def/append! (film x)
  [(inventory? x)
   (singular-join [film x] film_id)]
  [(rental? x)
   (film (inventory x))])

(def/append! (customer x)
  [(rental? x)
   (singular-join [customer x] customer_id)])

(define (rental-feed)
  (from R rental
        (join F (film R))
        (join C (customer R))
        (order-by 'desc (rental_id R))
        (limit 10)
        (select (title F))
        (select (rental_date R))
        (select (first_name C))
        (select (last_name C))))

(go (rental-feed))

; OK, now we can append various filters
(go (from R (rental-feed)
          (where (= (store_id (inventory R))
                    1))))

(via! inventory #:link rental #:to store_id)

(go (from R (rental-feed)
          (where (= (store_id R)
                    1))))

(via! film #:link rental inventory #:to title)

(go (from R (rental-feed)
          (where (like (title R)
                       (val: "%strangelove%")))))

; The shape of the rental feed is the same even as we add filters that were
; not mentioned in the original definition.
; OK, now we can switch to aggregating.

(def/append! (rental x)
  [(payment? x)
   (singular-join [rental x] rental_id)])

(def/append! (inventory x)
  [(rental? x)
   (singular-join [inventory x] inventory_id)]
  [(payment? x)
   (inventory (rental x))])

(via! inventory #:link rental payment #:to film_id)
(via! rental #:link payment #:to rental_date)

(define-syntax-rule (grouped-join [a b] col)
  (join x (a)
        (join-on (= (col x)
                    (col b)))
        (group-by (col x))))

(def/append! (payments-by x)
  [(rental? x)
   (grouped-join [payment x] rental_id)]
  [(inventory? x)
   (grouped-join [payment x] inventory_id)]
  [(film? x)
   (grouped-join [payment x] film_id)])

; This is more straightforward maybe:
#;(define (payment-report query)
    (from x query
          (join P (payments-by x))
          (define sales (sum (amount P)))
          (select sales)
          (select (count P))
          (select (/ sales (count P)))))
; ... but this is more flexible, right?:
(define (payment-report P)
  #;(-> (grouped-join-of payment?) (listof clause?))
  (let ([sales (sum (amount P))])
    ; TODO would be nice to avoid RS
    (RS list
        (select sales" as total_sales")
        (select (count P)" as num_rentals")
        (select (/ sales (count P))" as value_per_rental")
        (order-by 'desc sales)
        (limit 10))))


(go (from F film
          (select (title F))
          (payment-report (payments-by F))))

(go (from F film
          (select (title F))
          ; OUCH - I used from instead of join and of course it blew up:
          (payment-report (join P (payments-by F)
                                (where (> (rental_date P)
                                          (val: "2005-06-01")))
                                (where (< (rental_date P)
                                          (val: "2005-07-01")))))
          ; So how can I protect against that mistake?
          ))

; Perhaps even show a macro, like
#;(with-payment-report P F
    (where (> (rental_date P)
              (val: "2005-06-01")))
    (where (< (rental_date P)
              (val: "2005-07-01"))))



;;; NOTES
;What if we go
;1) show some rentals
;2) show how to join to film and customer (otherwise rental is boring)
;3) show how to filter for rentals by combinations of [store, customer, genre, actor]
;4) show how to group and aggregate
;Ideas
;* Sales by customer, for given date range
;* Sales by genre, for a given date range
;* Can generalize to "sales by X" where X in [Store, Customer, Genre, Film, Actor, Language, day of week]
;* Best customers for a given date range
;* Best-renting movies for a date range
;* Best-renting genres for our store
;* derived column IsInStock and IsOverdue
;
;Stats
;* 16044 rentals: select count(*) from payment p inner join rental r on r.rental_id = p.rental_id
;* 958 unique films: select count(distinct film_id) from payment p inner join rental r on r.rental_id = p.rental_id inner join inventory i on i.inventory_id = r.inventory_id
;* 599 unique customers: select count(distinct r.customer_id) from payment p inner join rental r on r.rental_id = p.rental_id inner join inventory i on i.inventory_id = r.inventory_id
;NOTE: you can also count i.customer_id and get the same result
;* 2 unique stores
;* only 1 unique language... so maybe not
