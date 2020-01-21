#lang racket

; TODO need to determine what `(require plisqin)` does wrt strictness.
; For now, let's just go with the unsafe stuff for compatibility.

(require plisqin-lib)
(provide (all-from-out plisqin-lib))

(require (prefix-in |.| plisqin-lib/unsafe/operators))
(provide (all-from-out plisqin-lib/unsafe/operators))


; TODO let's just see how these work
(require (prefix-in m: morsel-lib))
(require (only-in morsel-lib/sql
                  to-sql limit join-type))

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
  (scalar "round("a", "b")"))

(provide from join to-sql limit join-type round)
