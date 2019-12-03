#lang racket

(module+ test
  (require rackunit)
  (check-equal? #f "TODO rewire define-schema tests"))

#;(module just-columns racket
    ; Note that if we only (provide sakila-1) things won't work when we append
    ; because the tables and other identifiers don't bind the same.
    ; Providing all-defined-out seems to work.
    (provide (all-defined-out))

    (require "schema.rkt")

    (define-schema sakila-1
      (table actor
             #:column
             actor_id
             first_name
             last_name
             last_update)
      (table address
             #:column
             address_id
             address1
             address2
             district
             city_id
             postal_code
             phone
             last_update)
      (table category
             #:column
             category_id
             category_name
             last_update)
      (table city
             #:column
             city_id
             city_name
             country_id
             last_update)
      (table country
             #:column
             country_id
             country_name
             last_update)
      (table customer
             #:column
             customer_id
             store_id
             first_name
             last_name
             email
             address_id
             active
             create_date
             last_update)
      (table film
             #:column
             film_id
             title
             description
             release_year
             language_id
             original_language_id
             rental_duration
             rental_rate
             length
             replacement_cost
             rating
             special_features
             last_update)
      (table film_actor
             #:column
             actor_id
             film_id
             last_update)
      (table film_category
             #:column
             film_id
             category_id
             last_update)
      (table inventory
             #:column
             inventory_id
             film_id
             store_id
             last_update)
      (table language
             #:column
             language_id
             language_name
             last_update)
      (table payment
             #:column
             payment_id
             customer_id
             staff_id
             rental_id
             amount
             payment_date
             last_update)
      (table rental
             #:column
             rental_id
             rental_date
             inventory_id
             customer_id
             return_date
             staff_id
             last_update)
      (table staff
             #:column
             staff_id
             first_name
             last_name
             address_id
             picture
             email
             store_id
             active
             username
             password
             last_update)
      (table store
             #:column
             store_id
             manager_staff_id
             address_id
             last_update)))

#;(require "schema.rkt"
           rackunit
           'just-columns
           (only-in "../api.rkt"
                    scalar? join? join join-on RS))

#;(define-schema sakila-2
    #:append-to sakila-1
    (table film
           #:has-one
           [language #:using language_id]
           [original_language
            (join l (language)
                  (RS join-on (language_id l)" = "(original_language_id this)))]
           #:property
           [language_name
            (language_name (language this))]
           [original_language_name
            (language_name (original_language this))]))

#;(module+ test
    (require rackunit)
    (check-true (scalar? (actor_id (actor))))
    (check-true (join? (language (film))))
    (check-true (join? (original_language (film))))
    (check-true (scalar? (language_name (film))))
    (check-true (scalar? (original_language_name (film)))))
