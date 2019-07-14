#lang racket
(require db)
(provide connect-sakila)

(define here-dir
  (simplify-path (build-path (syntax-source #'here) "..")))

(define (connect-sakila)
  (parameterize ([current-directory here-dir])
    (sqlite3-connect
     #:database "sakila.db"
     #:mode 'read-only)))