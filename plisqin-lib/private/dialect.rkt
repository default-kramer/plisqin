#lang racket

(provide postgres postgres? mssql mssql? sqlite sqlite?
         dialect? current-dialect infer-dialect)

(require (prefix-in db: db))

(define dialect?
  (or/c 'postgres 'mssql 'sqlite #f))

(define-syntax-rule (def-dialect name name?)
  (begin
    (define (name) 'name)
    (define (name? x)
      (equal? 'name x))))
(def-dialect postgres postgres?)
(def-dialect mssql mssql?)
(def-dialect sqlite sqlite?)

(define current-dialect (make-parameter #f))

(define/contract (infer-dialect x)
  (-> (or/c db:connection? db:dbsystem?) (or/c dialect? #f))
  (if (db:connection? x)
      (infer-dialect (db:connection-dbsystem x))
      (match (db:dbsystem-name x)
        ; TODO why not just make these match?
        ['postgresql 'postgres]
        ['sqlite3 'sqlite]
        [else #f])))
