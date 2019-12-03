#lang racket

#;(provide make-statement
           def-statement)

(module+ test
  (require rackunit)
  (check-equal? #f "TODO rewire these tests"))

#;(module+ test
    (require "api.rkt" "schema.rkt" "show-table.rkt"
             rackunit)
    (def-table Country)
    (def-fields-of Country
      CountryId CountryName CountryPopulation)

    (current-connection 'cities-example)
    (define conn (current-connection))

    (def-statement (get-country id)
      (define testdef "blah")
      (void testdef)
      (RS from c Country
          (where (CountryId c)" = "id)))

    (define rows
      (db:query-rows conn (get-country 1)))
    (check-equal? (length rows) 1)

    (set! rows
          (db:query-rows conn (get-country -1)))
    (check-equal? (length rows) 0)

    ; Note - there is an opportunity for
    #;((get-country 1) #:conn 'default)
    ; to mean "execute the bound-statement using the current connection."
    ; And the #:conn parameter is optional of course.
    ; The ctx would need to hold a callback representing which of query-rows, query-exec, etc... you want to do

    (db:disconnect conn))
