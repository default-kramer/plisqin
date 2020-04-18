#lang racket

(provide connect-adventure-works show-table)

(require db
         (prefix-in p: plisqin-lib/private2/show-table))

(define here-dir
  (simplify-path (build-path (syntax-source #'here) "..")))

(define (connect-adventure-works)
  (parameterize ([current-directory here-dir])
    (sqlite3-connect
     #:database "AdventureWorks.db"
     #:mode 'read-only)))

(define (show-table q)
  (p:show-table-internal q connect-adventure-works))
