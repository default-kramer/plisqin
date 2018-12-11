#lang racket
(require (for-syntax racket "rewrite-lib.rkt")
         (prefix-in racket/ racket))
(provide (for-syntax rewrite)
         #%app)

; Need to define our custom #%app here so that it gets attached
; to the rewritten syntax objects
(define-syntax (#%app stx)
  (syntax-case stx ()
    [{_ val}
     (if (braced? stx)
         #'(app-or-return val)
         #'(racket/#%app val))]
    [(_ stuff ...)
     #'(racket/#%app stuff ...)]))

(define-syntax-rule (app-or-return val)
  (let ([x val])
    (if (procedure? x)
        (x)
        x)))

(define-syntax-rule (do-dot val func args ...)
  (let ([v val]
        [f func])
    (if (procedure? v)
        (raise-syntax-error #f "left side of dot cannot be a procedure" #'val)
        (f v args ...))))

; These definitions need to live inside a (begin-for-syntax ...) but that makes them
; hard to test. So duplicate the definitions into the test submodule.
(define-syntax-rule (make-testable forms ...)
  (begin
    (begin-for-syntax forms ...)
    (module+ test forms ...)))

(module+ test (require "rewrite-lib.rkt"))

(make-testable
 (define/contract (undot stx)
   (-> syntax? (or/c syntax? #f))
   (if (and (identifier? stx)
            (equal? #\. (string-ref (~a (syntax->datum stx)) 0)))
       (datum->syntax stx
                      (string->symbol (substring (~a (syntax->datum stx)) 1))
                      stx stx)
       #f))

 (define (rule1 stx)
   (syntax-case stx ()
     [{val func rest ...}
      (cond [;(and (undot #'val) (undot #'func))
             (undot #'val)
             (raise-syntax-error #f "Requires a value on the left side" #'val)]
            [(undot #'func)
             #`{(do-dot val #,(undot #'func)) rest ...}]
            [else #f])]
     [else #f]))

 (define (rule2 stx)
   (syntax-case stx ()
     [{val {func . args} . rest}
      (cond [(undot #'func)
             #`{(do-dot val #,(undot #'func) . args) . rest}]
            [else #f])]
     [else #f]))

 (define the-rules
   (/filter braced?
            (/or rule2 rule1)))

 (define/contract (rewrite stx)
   (-> syntax? syntax?)
   (apply-rules the-rules stx)))

(module+ test
  (require rackunit)
  (define (check/eval stx expected)
    (check-equal? (eval-syntax (rewrite stx)) expected))
  (check/eval #'{10 .add1 .add1}
              12)
  (check/eval #'{if 10 .add1 (.equal? 11) 'yep 'nope}
              'yep)
  ; If we don't use braces, it should be a no-op:
  (define stx #'(if 10 .add1 (.equal? 11) 'yep 'nope))
  (check-equal? (syntax->datum (rewrite stx))
                (syntax->datum stx)))