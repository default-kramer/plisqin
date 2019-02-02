#lang racket
(provide + - * /
         = <> like not-like
         < <= > >=)

(require (prefix-in r: racket))
(require "core.rkt")

;; If we have {x like "foo"} we want to render "(x like 'foo')"
;; Token -> Token
(define (format token)
  (if (string? token)
      (sql "'"token"'")
      token))

(define (do-binop symbol a b fragment-ctor)
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
     (fragment-ctor "("(format a)" "(~a symbol)" "(format b)")")]))

(define-syntax-rule (def-op OP racket-version ellipsis fragment-ctor)
  (define (OP . args)
    (match args
      [(list a b rest ellipsis)
       #:when (or (token? a) (interval? a)
                  (token? b) (interval? b))
       (apply OP (cons (do-binop 'OP a b fragment-ctor) rest))]
      [(list a)
       #:when (token? a) a]
      [else
       (apply racket-version args)])))
(def-op + r:+ ... scalar)
(def-op - r:- ... scalar)
(def-op * r:* ... scalar)
(def-op / r:/ ... scalar)
(def-op = r:= ... bool)
(def-op < r:< ... bool)
(def-op > r:> ... bool)
(def-op <= r:<= ... bool)
(def-op >= r:>= ... bool)

; If there is no builtin, it's easy
(define-syntax-rule (def-fresh-binop name joiner)
  (define/contract (name a b)
    (-> sql-token? sql-token? sql-token?)
    (bool "("(format a) joiner (format b)")")))
(def-fresh-binop <> " <> ")
(def-fresh-binop like " like ")
(def-fresh-binop not-like " not like ")