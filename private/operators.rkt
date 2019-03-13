#lang racket
(provide + - * / ||
         = <> like not-like is is-not
         < <= > >=
         plisqin-and plisqin-or plisqin-not)

(module+ test
  (require rackunit))

(require (prefix-in r: racket))
(require "core.rkt")

;; If we have {x like "foo"} we want to render "(x like 'foo')"
;; Token -> Token
(define (fmt token)
  (if (string? token)
      (RS scalar "'" (raw-sql token) "'")
      token))

; These x-ish? checks are where type checking could get added.
(define number-ish?
  (or/c number?
        token?))

(define string-ish?
  (or/c string?
        token?))

;; token? -> (listof interval?) -> token?
(define (+:date d intervals)
  (if (empty? intervals)
      d
      (dateadd d (+:interval intervals))))

;; token? -> (listof interval?) -> token?
(define (-:date d intervals)
  ; Add up all intervals, then negate the result
  (dateadd d (interval-negate (+:interval intervals))))

;; (listof interval?) -> interval?
(define (+:interval intervals)
  (match intervals
    [(list a) a]
    [(list a b rest ...)
     (+:interval (cons (interval-plus a b)
                       rest))]))

;; (listof interval?) -> interval?
(define (-:interval intervals)
  (match intervals
    [(list a) a]
    [(list a b rest ...)
     (-:interval (cons (interval-minus a b)
                       rest))]))

(define (interpose xs combiner)
  (match xs
    [(list a b rest ...)
     (list* (fmt a)
            combiner
            (interpose (cdr xs)
                       combiner))]
    [(list a) (list (fmt a))]
    [(list) (list)]))

(module+ test
  (check-equal? (interpose '(1 2 3) 'x)
                '(1 x 2 x 3)))

(define (check-all arg-list predicate? name expected)
  (for/fold ([count 0])
            ([arg arg-list])
    (when (not (predicate? arg))
      (apply raise-argument-error
             (list* name
                    expected
                    count
                    arg-list)))
    (values (add1 count))))

; || is string concatenation.
; In Racket, || is a zero-character identifier, so this might not be the wisest choice...
(define (|| str1 str2 . args)
  (set! args (list* str1 str2 args))
  (check-all args string-ish?
             '||
             "a string expression")
  ; the symbol 'concat is understood by to-sql based on the dialect
  (RS scalar "("(interpose args 'concat)")"))

(define (+ . args)
  (if (empty? args)
      (r:+)
      (match args
        ; Interval in first position. All arguments must be intervals.
        [(list a rest ...)
         #:when (interval? a)
         (check-all args interval?
                    '|+ (plisqin, intervals)|
                    "interval?")
         (+:interval args)]
        ; Interval in second position. Assume the first argument is a date.
        [(list a b rest ...)
         #:when (interval? b)
         (+:date a (cons b rest))]
        ; Token anywhere. Assume it is numeric until we add a type system.
        ; All arguments must be numbers.
        [args
         #:when (findf token? args)
         (check-all args number-ish?
                    '|+ (plisqin, numeric)|
                    "a numeric expression")
         (RS scalar "("(interpose args " + ")")")]
        [else
         (apply r:+ args)])))

(define (- . args)
  (if (empty? args)
      (r:-)
      (match args
        ; Interval in first position. All arguments must be intervals.
        [(list a rest ...)
         #:when (interval? a)
         (check-all args interval?
                    '|- (plisqin, intervals)|
                    "interval?")
         (-:interval args)]
        ; Interval in second position. Assume the first argument is a date.
        [(list a b rest ...)
         #:when (interval? b)
         (-:date a (cons b rest))]
        ; Token anywhere. Assume it is numeric. All arguments must be numbers.
        [args
         #:when (findf token? args)
         (check-all args number-ish?
                    '|- (plisqin, numeric)|
                    "a numeric expression")
         (RS scalar "("(interpose args " - ")")")]
        [else
         (apply r:- args)])))

; Multiply and divide are similar enough to use a macro
(define-syntax-rule (def-mul-div op racket-op joiner)
  (define (op . args)
    (if (empty? args)
        (racket-op)
        (match args
          ; Future: maybe report errors if dates/intervals are found?
          ;
          ; Token anywhere. Assume it is numeric. All arguments must be numbers.
          [args
           #:when (findf token? args)
           (check-all args number-ish?
                      (string->symbol (format "~a (plisqin, numeric)" 'op))
                      "a numeric expression")
           (RS scalar "("(interpose args joiner)")")]
          [else
           (apply racket-op args)]))))

(RS def-mul-div * r:* " * ")
(RS def-mul-div / r:/ " / ")

(define-syntax-rule (def-cmp op racket-op)
  (define (op arg1 . args)
    (set! args (cons arg1 args))
    (define has-token? (findf token? args))
    (cond
      [(and has-token?
            (not (= 2 (length args))))
       (apply raise-arity-error (list* 'op 2 args))]
      [has-token?
       (check-all args sql-token?
                  (string->symbol (format "(from plisqin) ~a" 'op))
                  "sql-token?")
       (bool (RS "(") (interpose args (raw-sql (format " ~a " 'op))) (RS ")"))]
      [else
       (apply racket-op args)])))

(def-cmp = r:=)
(def-cmp < r:<)
(def-cmp > r:>)
(def-cmp <= r:<=)
(def-cmp >= r:>=)

; If there is no builtin, it's easy
(define-syntax-rule (def-fresh-binop name joiner)
  (define/contract (name a b)
    (-> sql-token? sql-token? sql-token?)
    (RS bool "("(fmt a) joiner (fmt b)")")))
(RS def-fresh-binop <> " <> ")
(RS def-fresh-binop like " like ")
(RS def-fresh-binop not-like " not like ")

(define/contract (is a b)
  (-> (or/c sql-token? 'null) (or/c sql-token? 'null) sql-token?)
  (define (f x)
    (fmt (if (eq? 'null x)
             (RS scalar "null")
             x)))
  (RS bool "("(f a)" is "(f b)")"))

; SQLite allows "1 is not 2" but SQL Server does not.
; Simply negating the result of "is" should work in all DBs.
(define/contract (is-not a b)
  (-> (or/c sql-token? 'null) (or/c sql-token? 'null) sql-token?)
  (plisqin-not (is a b)))


; "and" and "or" are intended to be used by #lang plisqin as infix operators,
; so they take only 2 arguments. A more rackety version could accept many.
(define/contract (plisqin-and a b)
  (-> sql-token? sql-token? sql-token?)
  (RS bool "("a" and "b")"))
(define/contract (plisqin-or a b)
  (-> sql-token? sql-token? sql-token?)
  (RS bool "("a" or "b")"))
(define/contract (plisqin-not a)
  (-> sql-token? sql-token?)
  (RS bool "not "a))
