#lang racket
(require (for-syntax racket))
(require (prefix-in rewriter/ "rewriter.rkt")
         (prefix-in runtime/ "runtime.rkt")
         syntax/strip-context)

(provide (except-out (all-from-out racket)
                     #%module-begin read-syntax #%app #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [reader read-syntax]
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

; A readtable extension that gets invoked when the reader encounters a dot.
; We have to decide if the dot is
; 1) chained to the next token, like ".foo"
; 2) unchained, like ". foo"
; 3) multiple dots, like "..."
; This should return one syntax object in all cases
(define (read-dot char port name line col pos)
  (define this-dot (datum->syntax #f '|.| (list name line col pos 1)))
  (define next-char (peek-char port))
  (define (make-single-dot chained?)
    (let ([stx (datum->syntax #f '|.|
                              (list name line col pos 1))])
      (syntax-property stx rewriter/stx-prop-chained chained?)))
  (if (or (char-whitespace? next-char)
          (member next-char non-id-starters))
      ; return single dot, unchained
      (make-single-dot #f)
      ; else
      (let ([dot-count (read-dots port)])
        ; If we have more than 1 consecutive dot, treat it literally (useful for ellipsis)
        (if (dot-count . > . 1)
            ; return multiple dots
            (datum->syntax #f (string->symbol (make-string dot-count #\.))
                           (list name line col pos dot-count))
            ; else return single dot, chained
            (make-single-dot #t)))))

(define (customize-readtable)
  (make-readtable (current-readtable) #\. 'terminating-macro read-dot))

; Returns list of syntax objects
(define (read-all source-name in)
  (define (helper source-name in accum)
    (define result (read-syntax source-name in))
    (if (not (eof-object? result))
        (helper source-name in (cons result accum))
        (reverse accum)))
  (parameterize ([current-readtable (customize-readtable)])
    (helper source-name in '())))

(define (reader source-name in)
  (define stxs (read-all source-name in))
  (define result `(module my-mod plisqin/private/lang/reader
                    (require plisqin/private/lang/default-require)
                    (module configure-runtime racket
                      (require (submod plisqin/private/lang/reader runtime-help))
                      (lang-plisqin-setup!))
                    ,@stxs))
  (set! result (datum->syntax #f result))
  ; strip-context per https://groups.google.com/forum/#!topic/racket-users/DFBnCqLg0Xk
  (set! result (strip-context result))
  result)

(define (read-repl source-name in)
  (define result
    (parameterize ([current-readtable (customize-readtable)]
                   [read-accept-lang #f])
      (read-syntax source-name in)))
  (if (eof-object? result)
      result
      ; TODO Not sure if strip-context is desired here...
      (rewriter/rewrite (strip-context result))))

(module+ runtime-help
  (provide lang-plisqin-setup!)
  (define (lang-plisqin-setup!)
    (current-read-interaction read-repl)))

(define-syntax (module-begin stx)
  (define result
    (syntax-case stx ()
      [(_ rest ...)
       #`(#%module-begin rest ...)]))
  (define transformed (rewriter/rewrite result))
  ;(println transformed)
  transformed)

(define-syntax (top-interaction stx)
  ; If we type {a = b} into the REPL, we will get
  ;   {#%top-interaction a = b}
  ; Before we pass it to the rewriter, we want to transform it back to
  ;   {a = b}
  (set! stx (datum->syntax stx
                           (cdr (syntax-e stx))
                           stx stx))
  ;(println stx)
  (set! stx (rewriter/rewrite stx))
  ;(println stx)
  #`(#%top-interaction . #,stx))