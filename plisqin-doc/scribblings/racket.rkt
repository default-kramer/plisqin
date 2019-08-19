#lang racket
; Provide everything from racket except identifiers that clash with plisqin.
(define-for-syntax plisqin-conflicts
  (list "group-by" "count" "min" "max" "second" "case" "null"
        "+" "-" "/" "*" "=" "<" "<=" ">" ">=" "this"))

(require racket/provide)
(provide (filtered-out
          (λ(name) (if (member name plisqin-conflicts)
                       #f
                       name))
          (all-from-out racket)))