#lang racket

(provide show-table-internal)

(require (prefix-in db: db)
         "rows-result-to-string.rkt"
         (only-in "_core.rkt" query? to-sql))

(define/contract (show-table-internal x conn)
  (-> (or/c query? string?)
      (or/c db:connection? (-> db:connection?))
      any/c)
  (if (procedure? conn)
      (let ([conn (conn)])
        (show-table-internal x conn)
        (db:disconnect conn))
      (let* ([sql (cond
                    [(query? x) (to-sql x)]
                    [(string? x) x]
                    [else (error "TODO")])]
             [result (db:query conn sql)])
        (displayln (rows-result->string result)))))
