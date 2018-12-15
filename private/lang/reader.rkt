#lang racket
(require (for-syntax racket))
(require (prefix-in rewriter/ "rewriter.rkt")
         (prefix-in runtime/ "runtime.rkt"))

(provide (except-out (all-from-out racket)
                     #%module-begin read-syntax #%app)
         (rename-out [module-begin #%module-begin]
                     [read-syn read-syntax]
                     [runtime/#%app #%app]
                     [runtime/#%do-dot #%do-dot]))

(read-accept-dot #f)
(read-accept-infix-dot #f)

; Read more dots and return the count, assuming we have already read one.
(define (read-dots port [count 1])
  ; port -> int
  (let ([maybe-another-dot (peek-char port)])
    (if (equal? #\. maybe-another-dot)
        (begin
          (read-char port)
          (read-dots port (add1 count)))
        count)))

(define non-id-starters (string->list "(){}[]"))

(define (raise-dot-error stx)
  (raise-syntax-error #f "Dot must be followed by an identifier" stx))

(define (read-dot char port name line col pos)
  (define this-dot (datum->syntax #f '|.| (list name line col pos 1)))
  (define next-char (peek-char port))
  (if (or (char-whitespace? next-char)
          (member next-char non-id-starters))
      (raise-dot-error this-dot)
      ; else
      (let ([dot-count (read-dots port)])
        ; If we have more than 1 consecutive dot, treat it literally (useful for ellipsis)
        (if (dot-count . > . 1)
            (datum->syntax #f (string->symbol (make-string dot-count #\.))
                           (list name line col pos dot-count))
            ; else
            (let ([next (read-syntax name port)])
              (cond
                [(eof-object? next)
                 (raise-dot-error this-dot)]
                [(not (identifier? next))
                 (raise-dot-error this-dot)]
                [else
                 (datum->syntax next
                                (string->symbol (format ".~a" (syntax->datum next)))
                                next next)]))))))

(define (customize-readtable)
  (make-readtable (current-readtable) #\. 'terminating-macro read-dot))

(define (reader source-name in accum)
  (define result (read-syntax source-name in))
  (if (not (eof-object? result))
      (reader source-name in (cons result accum))
      (begin
        (set! result `(module my-mod plisqin/private/lang/reader
                        ,@(reverse accum)))
        (set! result (datum->syntax #f result))
        ;(set! result (quick-rewrite result))
        (set! result (rewriter/rewrite result))
        result)))

(define (read-syn source-name in)
  (parameterize ([current-readtable (customize-readtable)])
    (reader source-name in '())))

(define-syntax (module-begin stx)
  (define result
    (syntax-case stx ()
      [(_ rest ...)
       #`(#%module-begin rest ...)]))
  (define transformed result); (rewriter/rewrite result))
  ;(println transformed)
  transformed)