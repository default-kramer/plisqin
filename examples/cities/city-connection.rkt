#lang racket
(require db)
(provide connect-cities)

(define (connect-cities)
  (parameterize ([current-directory (syntax-source #'here)])
    ; I have no idea if this will work on non-Windows.
    (sqlite3-connect
     #:database "../cities.db"
     #:mode 'read-only)))