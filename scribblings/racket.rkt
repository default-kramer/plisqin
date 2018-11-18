#lang racket
; Provide everything from racket except identifiers that clash with plisqin.

; For some reason, this doesn't hide stuff from racket/base:
(void '(provide (except-out (all-from-out racket)
                            group-by count min max)))

; So do it this way instead:
(require racket/provide)
(provide (filtered-out
          (λ(name) (if (member name '("group-by" "count" "min" "max"))
                       #f
                       name))
          (all-from-out racket)))