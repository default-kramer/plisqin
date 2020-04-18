#lang racket

; TODO this is still up for debate
(require plisqin-lib/types
         plisqin-lib/dialect
         plisqin-lib/strict
         (prefix-in |.| plisqin-lib/strict/operators)
         (prefix-in % plisqin-lib/loose)
         (prefix-in % plisqin-lib/loose/operators)
         (prefix-in %% plisqin-lib/unsafe)
         (prefix-in %% plisqin-lib/unsafe/operators))

(provide (all-from-out plisqin-lib/types
                       plisqin-lib/dialect
                       plisqin-lib/strict
                       plisqin-lib/strict/operators
                       plisqin-lib/loose
                       plisqin-lib/loose/operators
                       plisqin-lib/unsafe
                       plisqin-lib/unsafe/operators))

; TODO this need attention
(require plisqin-lib)
(provide (all-from-out plisqin-lib))

; TODO these definitely need to get moved into plisqin-lib
(provide from join to-sql limit offset distinct join-type ??)

; TODO let's just see how these work
(require plisqin-lib/private2/from)
(require (only-in plisqin-lib/private2/_core
                  to-sql limit offset distinct join-type))

(define (?? token fallback)
  (>> token #:fallback fallback))
