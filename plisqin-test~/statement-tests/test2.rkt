#lang racket

(require rackunit
         plisqin-lib/dialect
         plisqin-examples/adventure-works
         (prefix-in db: db)
         plisqin-lib/private2/statement/compile-statements)

(compile-statements #:module "test1.rkt"
                    #:dialect (sqlite))

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
; To test other DBs, don't forget to change the dialect above.
; And if a test fails, make sure the expected data is actually in the DB!
#;(define conn (db:postgresql-connect #:user "postgres"
                                      #:password "postgres"
                                      #:database "adventure_works"
                                      #:server "localhost"
                                      #:port 5432))
#;(define conn (db:odbc-connect #:dsn "AdventureWorks"
                                #:user "AdventureWorks"
                                #:password "AdventureWorks"))


; == Test get-category ==
(check-equal? (rowcount (get-category 1 "Bikes"))
              1)
(check-equal? (rowcount (get-category 1 "wrong category name"))
              0)


; == Test foo ==
; If all the args are 1, we should get 1 row from the DB.
; If any arg differs, we should get 0 rows from the DB.
; First test with just the mandatory args:
(check-equal? (rowcount (foo 1 1 #:d 1 #:e 1))
              1)
(check-equal? (rowcount (foo 0 1 #:d 1 #:e 1))
              0)
(check-equal? (rowcount (foo 1 0 #:d 1 #:e 1))
              0)
(check-equal? (rowcount (foo 1 1 #:d 0 #:e 1))
              0)
(check-equal? (rowcount (foo 1 1 #:d 1 #:e 0))
              0)
; Now add the optional non-keyword arg:
(check-equal? (rowcount (foo 1 1 4 #:d 1 #:e 1))
              0)
(check-equal? (rowcount (foo 1 1 1 #:d 1 #:e 1))
              1)
; Now add the optional keyword arg:
(check-equal? (rowcount (foo 1 1 1 #:d 1 #:e 1 #:f 99))
              0)
(check-equal? (rowcount (foo 1 1 1 #:d 1 #:e 1 #:f 1))
              1)
; Make sure that we cannot omit the required args.
(check-exn exn:fail:contract?
           (lambda () (foo 1 1 #:d 1)))
(check-exn exn:fail:contract?
           (lambda () (foo 1 1 #:e 1)))
(check-exn exn:fail:contract?
           (lambda () (foo 1 1)))
(check-exn exn:fail:contract:arity?
           ; Perhaps an implementation detail within Racket that this one raises
           ; exn:fail:contract:arity while the rest just raise exn:fail:contract
           (lambda () (foo 1)))
(check-exn exn:fail:contract?
           (lambda () (foo 1 #:d 1 #:e 1)))


(db:disconnect conn)
