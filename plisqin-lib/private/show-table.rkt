#lang racket
(provide show-table current-connection)

(require (prefix-in db: db)
         "rows-result-to-string.rkt"
         "../examples/cities/city-connection.rkt")

(define/contract (guard-current-connection x)
  (-> (or/c #f 'cities-example db:connection?)
      (or/c #f db:connection?))
  (match x
    ['cities-example (connect-cities)]
    [else x]))

(define current-connection
  (make-parameter #f guard-current-connection))

(define (show-table x)
  #;(-> (or/c query? string?) any/c)
  #;(OLD_CODE
     (define conn (current-connection))
     (when (not (db:connection? conn))
       (error "current-connection is not set"))
     (define sql
       (cond
         [(query? x) (to-sql x)]
         [(string? x) x]
         [else (error "TODO")]))
     (define result (db:query conn sql))
     (displayln (rows-result->string result)))
  (displayln "TODO show-table needs to be reimplemented"))
