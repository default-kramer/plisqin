#lang racket

(require plisqin
         "schema.rkt")

(define-statement (get-category #:name [name String?])
  (from cat ProductCategory
        (where (.= (Name cat)
                   (?? name /void)))
        (select (ProductCategoryID cat))
        (select (Name cat))
        (select (ModifiedDate cat))))
