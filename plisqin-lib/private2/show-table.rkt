#lang racket
(provide show-table current-connection)

(require (prefix-in db: db)
         "rows-result-to-string.rkt"
         (only-in morsel-lib query?)
         (only-in morsel-lib/sql to-sql))

(define/contract (guard-current-connection x)
  (-> (or/c #f db:connection?)
      (or/c #f db:connection?))
  x)

(define current-connection
  (make-parameter #f guard-current-connection))

(define/contract (show-table x)
  (-> (or/c query? string?) any/c)
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
