#lang racket

(module+ test
  (require rackunit)
  (check-equal? #f "TODO rewire these tests"))

#;(test
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
