#lang racket

(require plisqin
         plisqin-lib/private2/statement/define-statement)

(define-statement (get-category [id Number]
                                [name String])
  ; use the second parameter before the first to test that we handle it correctly
  (from cat "ProductCategory"
        (%%where cat".Name = "name)
        (%%where cat".ProductCategoryID = "id)
        (%%where cat".ProductCategoryID = "id)
        (%%select "ProductCategoryID, Name, ModifiedDate")))

(module+ test
  (require rackunit)

  (check-equal? (procedure-arity get-category)
                2)

  (let ([id (val 1)]
        [name (val "Bikes")])
    (check-equal? (get-category id name)
                  ; copy-paste the body from define-statement:
                  (from cat "ProductCategory"
                        (%%where cat".Name = "name)
                        (%%where cat".ProductCategoryID = "id)
                        (%%where cat".ProductCategoryID = "id)
                        (%%select "ProductCategoryID, Name, ModifiedDate")))))
