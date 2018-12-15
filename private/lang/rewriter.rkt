#lang racket
(require (for-syntax racket "rewrite-lib.rkt")
         "rewrite-lib.rkt")
(provide (for-syntax rewrite) rewrite)

; Duplicate the definitions into begin-for-syntax and the test submodule.
(define-syntax-rule (define-everywhere forms ...)
  (begin
    (begin-for-syntax forms ...)
    forms ...
    (module+ test forms ...)))

(module+ test (require "rewrite-lib.rkt"))

(define-everywhere
  (define/contract (undot stx)
    (-> syntax? (or/c syntax? #f))
    (if (and (identifier? stx)
             (equal? #\. (string-ref (~a (syntax->datum stx)) 0)))
        (datum->syntax stx
                       (string->symbol (substring (~a (syntax->datum stx)) 1))
                       stx stx)
        #f))

  (define/contract (make-infix-rewriter symbols)
    (-> (listof symbol?) procedure?)
    (λ(stx)
      (syntax-case stx ()
        [{a op b rest ...}
         (begin
           (define loc (second (syntax->list stx)))
           (cond [(member (syntax-e #'op) symbols)
                  ;#`{(op a b) rest ...}
                  (datum->syntax stx
                                 (cons
                                  (datum->syntax stx
                                                 (list #'op #'a #'b)
                                                 loc stx)
                                  (syntax->list #'(rest ...)))
                                 loc stx)]
                 [else #f]))]
        [else #f])))

  (define (dot-id stx)
    ; rewrite a.b to (#%do-dot a b)
    (syntax-case stx ()
      [{val func rest ...}
       (cond [(undot #'func)
              (define loc (second (syntax->list stx)))
              ;#`{(#%do-dot val #,(undot #'func)) rest ...}
              (datum->syntax stx
                             (cons
                              (datum->syntax stx
                                             (list
                                              (datum->syntax stx '#%do-dot loc stx)
                                              #'val
                                              (undot #'func))
                                             loc stx)
                              (syntax->list #'(rest ...)))
                             loc stx)]
             [else #f])]
      [else #f]))

  (define (valueless-dot stx)
    ; detect ".foo" without a value to the left of it
    (syntax-case stx ()
      [{dotted rest ...}
       (cond [(undot #'dotted)
              (raise-syntax-error #f "Dotted expression missing a value on the left side" #'dotted)]
             [else #f])]
      [else #f]))

  (define (dot-list stx)
    ; rewrite a(.foo bar ...) to (#%do-dot a foo bar ...)
    (syntax-case stx ()
      [{val {func args ...} rest ...}
       (cond [(undot #'func)
              ;#`{(#%do-dot val #,(undot #'func) args ...) rest ...}
              (define expr (second (syntax->list stx)))
              (datum->syntax stx
                             (cons (datum->syntax expr
                                                  (list* '#%do-dot #'val (undot #'func)
                                                         (syntax->list #'(args ...)))
                                                  expr expr)
                                   (syntax->list #'(rest ...)))
                             stx stx)]
             [else #f])]
      [else #f]))

  (define-syntax-rule (/pass rules ...)
    (/pass-ltr (/filter braced? (/or rules ...))))

  (define the-rewriter
    (/all
     (/pass valueless-dot
            dot-list
            dot-id)
     (/pass (make-infix-rewriter '(* /)))
     (/pass (make-infix-rewriter '(+ -)))
     (/pass (make-infix-rewriter '(=)))))

  (define/contract (rewrite stx)
    (-> syntax? syntax?)
    (or (the-rewriter stx) stx)))

(module+ test
  (require rackunit "runtime.rkt")
  ; This is needed for eval-syntax to work
  (dynamic-require "runtime.rkt" 0)

  (define (check/eval stx expected)
    (check-equal? (eval-syntax (rewrite stx)) expected))
  (check/eval #'{10 .add1}
              11)
  (check/eval #'{10 .add1 .add1}
              12)
  (check/eval #'{if 10 .add1 (.equal? 11) 'yep 'nope}
              'yep)
  ; If we don't use braces, it should be a no-op:
  (define stx #'(if 10 .add1 (.equal? 11) 'yep 'nope))
  (check-equal? (syntax->datum (rewrite stx))
                (syntax->datum stx))
  (check/eval #'{10 + 9}
              19)
  (check/eval #'{9 .add1 * 41 .add1}
              420)
  (check/eval #'{* 3 4}
              12))