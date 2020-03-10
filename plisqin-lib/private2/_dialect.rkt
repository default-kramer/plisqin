#lang racket

(provide (all-from-out morsel-lib/sql/dialect))

(require morsel-lib/sql/dialect
         (prefix-in db: db))

; TODO decide if this code is still useful:
(define (infer-dialect x)
  #;(-> (or/c db:connection? db:dbsystem?) (or/c dialect? #f))
  (if (db:connection? x)
      (infer-dialect (db:connection-dbsystem x))
      (match (db:dbsystem-name x)
        ['postgresql (postgres)]
        ['sqlite3 (sqlite)]
        [else #f])))
