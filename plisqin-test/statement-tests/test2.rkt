#lang racket

(require (for-syntax plisqin-lib/dialect)
         plisqin-examples/adventure-works
         (prefix-in db: db)
         plisqin-lib/private2/statement/compile-statements)

(compile-statements "test1.rkt" (sqlite))

(module+ test
  (require rackunit)

  ;(displayln (statement-sql get-category))

  (define (rowcount x)
    (cond
      [(db:rows-result? x)
       (length (db:rows-result-rows x))]
      [(db:statement? x)
       (rowcount (db:query conn x))]
      [else
       (error "bad arg:" x)]))

  (define conn (connect-adventure-works))
  ; To test other DBs, don't forget to change the dialect above:
  #;(define conn (db:postgresql-connect #:user "postgres"
                                        #:password "postgres"
                                        #:database "adventure_works"
                                        #:server "localhost"
                                        #:port 5432))
  #;(define conn (db:odbc-connect #:dsn "AdventureWorks"
                                  #:user "AdventureWorks"
                                  #:password "AdventureWorks"))

  (check-equal? 1
                (rowcount (get-category 1 "Bikes")))
  (check-equal? 0
                (rowcount (get-category 1 "wrong category name")))

  (db:disconnect conn))
