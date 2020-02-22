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
(provide from join to-sql limit offset distinct join-type round)


; TODO let's just see how these work
(require (prefix-in m: morsel-lib))
(require (only-in morsel-lib/sql
                  to-sql limit offset distinct join-type))

(define-for-syntax (tweak stx)
  (syntax-case stx (join)
    [(join a b clause ...)
     (quasisyntax/loc stx
       (m:attach a b #,@(map tweak (syntax->list #'(clause ...)))))]
    [_ stx]))

(define-syntax (from stx)
  (syntax-case stx ()
    [(_ a b clause ...)
     (quasisyntax/loc stx
       (m:from a b #,@(map tweak (syntax->list #'(clause ...)))))]))

(define-syntax (join stx)
  (syntax-case stx ()
    [(_ a b #:to c clause ...)
     (quasisyntax/loc stx
       (m:join a b #:to c #,@(map tweak (syntax->list #'(clause ...)))))]
    ; TODO we want #:to to be required, but leave it optional for now
    [(_ a b clause ...)
     (quasisyntax/loc stx
       (m:join a b #,@(map tweak (syntax->list #'(clause ...)))))]))

(define (round a b)
  (%%scalar "round("a", "b")"))
