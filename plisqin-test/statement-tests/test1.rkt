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

; This is to test every kind of argument we have
(define-statement (foo a
                       [b Number]
                       [c Number 1]
                       #:d d
                       #:e [e Number]
                       #:f [f Number 1]
                       )
  (from cat "ProductCategory"
        (%%where cat".ProductCategoryID = "a)
        (%%where cat".ProductCategoryID = "b)
        (%%where cat".ProductCategoryID = "c)
        (%%where cat".ProductCategoryID = "d)
        (%%where cat".ProductCategoryID = "e)
        (%%where cat".ProductCategoryID = "f)))

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
