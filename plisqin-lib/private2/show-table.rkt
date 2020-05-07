#lang racket

(provide show-table-internal)

(require (prefix-in db: db)
         "rows-result-to-string.rkt"
         (only-in "_core.rkt" query? to-sql)
         (only-in "_types.rkt" Token))

(define/contract (show-table-internal x conn)
  (-> (or/c query? Token string?)
      (or/c db:connection? (-> db:connection?))
      any/c)
  (if (procedure? conn)
      (let ([conn (conn)])
        (show-table-internal x conn)
        (db:disconnect conn))
      (let* ([sql (cond
                    [(string? x) x]
                    [else (to-sql x)])]
             [result (db:query conn sql)])
        (displayln (rows-result->string result)))))
