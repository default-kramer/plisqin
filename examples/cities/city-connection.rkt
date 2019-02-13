#lang racket
(require db)
(provide connect-cities)

(define here-dir
  (simplify-path (build-path (syntax-source #'here) "..")))

(define (connect-cities)
  (parameterize ([current-directory here-dir])
    (sqlite3-connect
     #:database "cities.db"
     #:mode 'read-only)))