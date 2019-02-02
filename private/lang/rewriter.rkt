#lang racket
(require (for-syntax racket "rewrite-lib.rkt")
         "rewrite-lib.rkt")
(provide (for-syntax rewrite) rewrite
         stx-prop-chained)

; Duplicate the definitions into begin-for-syntax.
(define-syntax-rule (define-everywhere forms ...)
  (begin
    (begin-for-syntax forms ...)
    forms ...))

(define-everywhere
  ; If the reader determines that a dot is chained to the next symbol,
  ; it should set this property to let us know.
  ; For example in ".bar" the dot is considered chained.
  ; In ". bar" the dot is not considered chained.
  (define stx-prop-chained 'plisqin-chained)

  ;; Syntax -> Any
  (define (chained? stx)
    (syntax-property stx stx-prop-chained))

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
      [{val dot func rest ...}
       (cond [(chained? #'dot)
              (define loc (third (syntax->list stx)))
              ;#`{(#%do-dot val #,(undot #'func)) rest ...}
              (datum->syntax stx
                             (cons
                              (datum->syntax stx
                                             (list
                                              (datum->syntax stx '#%do-dot loc stx)
                                              #'val
                                              #'func)
                                             loc stx)
                              (syntax->list #'(rest ...)))
                             loc stx)]
             [else #f])]
      [else #f]))

  (define (valueless-dot stx)
    ; detect ".foo" without a value to the left of it
    (syntax-case stx ()
      [{dot rest ...}
       (cond [(chained? #'dot)
              (raise-syntax-error #f "Dotted expression missing a value on the left side" #'dot)]
             [else #f])]
      [else #f]))

  (define (dot-list stx)
    ; rewrite a(.foo bar ...) to (#%do-dot a foo bar ...)
    (syntax-case stx ()
      [{val {dot func args ...} rest ...}
       (cond [(chained? #'dot)
              ;#`{(#%do-dot val #,(undot #'func) args ...) rest ...}
              (define expr (second (syntax->list stx)))
              (datum->syntax stx
                             (cons (datum->syntax expr
                                                  (list* '#%do-dot #'val #'func
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
     (/pass (make-infix-rewriter '(= < > <= >= <> like not-like)))))

  (define/contract (rewrite stx)
    (-> syntax? syntax?)
    (or (the-rewriter stx) stx)))