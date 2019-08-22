#lang racket
(require db)
(provide connect-adventure-works)

(define here-dir
  (simplify-path (build-path (syntax-source #'here) "..")))

(define (connect-adventure-works)
  (parameterize ([current-directory here-dir])
    (sqlite3-connect
     #:database "AdventureWorks.db"
     #:mode 'read-only)))