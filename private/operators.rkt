#lang racket
(provide + -)

(require (prefix-in r: racket))
(require "core.rkt")

(define (do-math symbol a b)
  (cond
    [(and (interval? a)
          (interval? b))
     (match symbol
       ['+ (interval-plus a b)]
       ['- (interval-minus a b)]
       [else (error "TODO illegal date math")])]
    [(interval? b)
     ; TODO warn if we can prove that a is not dateish.
     ; Perhaps in "strict mode" warn if we can't prove that a is dateish.
     (match symbol
       ['+ (dateadd a b)]
       ['- (dateadd a (interval-negate b))]
       [else (error "TODO illegal date math")])]
    [else
     (scalar "("a" "(~a symbol)" "b")")]))

(define-syntax-rule (def-op OP racket-version ellipsis)
  (define (OP . args)
    (match args
      [(list a b rest ellipsis)
       #:when (or (token? a) (interval? a)
                  (token? b) (interval? b))
       (apply OP (cons (do-math 'OP a b) rest))]
      [(list a)
       #:when (token? a) a]
      [else
       (apply racket-version args)])))
(def-op + r:+ ...)
(def-op - r:- ...)