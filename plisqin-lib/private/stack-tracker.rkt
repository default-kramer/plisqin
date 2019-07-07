#lang racket
(provide attach-callstacks callstack-get)

(module help racket
  (require "core.rkt")
  (provide (all-defined-out))
  (define callstack? (listof syntax?))

  (struct callstack-key () #:transparent)
  (define key (callstack-key))

  (define/contract (callstack-append token stx)
    (-> token? syntax? token?)
    (metadata-set key token
                  (cons stx (callstack-get token))
                  #:hidden? #t))

  (define/contract (callstack-get token)
    (-> token? callstack?)
    (metadata-get key token '()))

  (define/contract (maybe-callstack-append x stx)
    (-> any/c syntax? any/c)
    (if (token? x)
        (callstack-append x (match (syntax->list stx)
                              [(list app body ...)
                               #:when (equal? '#%app (syntax->datum app))
                               ; We can reuse the srclocs of stx - they point to the original body
                               (datum->syntax stx body stx)]
                              [else stx]))
        x)))
(require 'help)

; TODO I think this implementation might be wrong...
; We probably want to fall back to the caller's existing APP rather than racket's built-in.
(define-syntax-rule (old-app stuff ...) (#%app stuff ...))

(define-syntax (attach-callstacks stx)
  (define APP (datum->syntax stx '#%app))
  #`(begin
      (define-syntax (#,APP stx2)
        ; stx3 will be stx2 except with the leading APP swapped for old-app
        (define stx3 (syntax->list stx2))
        (set! stx3 (cons #'old-app (cdr stx3)))
        (set! stx3 (datum->syntax stx2 stx3 stx2))
        #`(old-app maybe-callstack-append #,stx3 #'#,stx2))))

(module+ test
  (require "core.rkt" rackunit)
  (attach-callstacks)

  (define (f x)
    (RS where "42 < "x" and "x" < 420"))

  (check-equal? (f 9) (f 9))
  (check-equal?
   (equal-hash-code (f 9))
   (equal-hash-code (f 9)))
  (check-equal?
   (syntax->datum (car (callstack-get (f 9))))
   '(f 9)))