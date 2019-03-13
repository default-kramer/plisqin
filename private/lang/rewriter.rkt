#lang racket
(require (for-syntax racket
                     "rewrite-lib.rkt"
                     syntax/readerr)
         "rewrite-lib.rkt"
         syntax/readerr)
(provide (for-syntax rewrite) rewrite
         stx-prop-dot-type)

; Duplicate the definitions into begin-for-syntax.
(define-syntax-rule (define-everywhere forms ...)
  (begin
    (begin-for-syntax forms ...)
    forms ...))

(define-everywhere
  ; This property will be set by the reader to one of two values:
  ; 1) 'delimited means that the dot is surrouned by whitespace.
  ;    The default reader gives special treatment to delimited dots,
  ;    see "1.3.6 Reading Pairs and Lists" of the Reference.
  ;    IMO, rest args are the most important use case here.
  ;    I don't care too much if plisqin doesn't support the other patterns.
  ; 2) 'chained means that the dot is immediately to the left of another
  ;    token. For example in {foo .bar} and {foo.bar} both dots are chained.
  ;    And we want to rewrite both to (bar foo)
  (define stx-prop-dot-type 'plisqin-dot-type)

  ;; Syntax -> Any
  (define (chained? stx)
    (eq? 'chained
         (syntax-property stx stx-prop-dot-type)))
  (define (delimited? stx)
    (eq? 'delimited
         (syntax-property stx stx-prop-dot-type)))

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

  (define/contract (make-unary-rewriter symbols)
    (-> (listof symbol?) procedure?)
    (λ(stx)
      (syntax-case stx ()
        [{op arg rest ...}
         (begin
           (define loc (first (syntax->list stx)))
           (cond [(member (syntax-e #'op) symbols)
                  ; rewrite to {(op arg) rest ...}
                  (let* ([inner #'(op arg)]
                         ; only copy srcloc, not other properties (like braces) to avoid infinite recursion
                         [inner (copy-srcloc loc inner)])
                    (datum->syntax stx
                                   (syntax->list #`{#,inner rest ...})
                                   stx stx))]
                 [else #f]))]
        [else #f])))

  ; Because we customized the read of the dot, (a b c . d)
  ; will read as (list a b c literally-a-dot d)
  ; We need to rewrite it to (cons a (cons b (cons c d)))
  (define (rest-args-pattern stx)
    (syntax-case stx ()
      [(args ... dot last)
       (if (delimited? #'dot)
           (datum->syntax stx
                          (append (syntax->list #'(args ...))
                                  #'last)
                          stx stx)
           #f)]
      [else #f]))

  ; Raises an error if any unresolved dots remain
  (define (dot-misuse stx)
    (if (or (delimited? stx)
            (chained? stx))
        ; #lang racket calls this a read error.
        ; For #lang plisqin, this happens in the expander (rewriter).
        ; Does it matter?
        (raise-read-error "plisqin: illegal use of `.`"
                          (syntax-source stx)
                          (syntax-line stx)
                          (syntax-column stx)
                          (syntax-position stx)
                          (syntax-span stx))
        #f))

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

  ; Searches expr for orig and substitutes it with replacement
  (define (replace orig replacement expr)
    (if (equal? expr orig)
        replacement
        (let ([content (syntax-e expr)])
          (cond
            [(list? content) (datum->syntax expr
                                            (map (curry replace orig replacement) content)
                                            expr expr)]
            [(pair? content) (datum->syntax expr
                                            (cons (replace orig replacement (car content))
                                                  (replace orig replacement (cdr content)))
                                            expr expr)]
            [else expr]))))

  (define (make-literal-rewriter from to)
    (λ(stx)
      (syntax-case stx ()
        [{lit rest ...}
         (if (equal? (syntax-e #'lit) from)
             (let ([replacement (datum->syntax stx to stx stx)])
               (replace #'lit replacement stx))
             #f)]
        [else #f])))

  (define-syntax-rule (/pass rules ...)
    (/pass-ltr (/filter braced? (/or rules ...))))

  (require (only-in "../operators.rkt"
                    plisqin-and
                    plisqin-or
                    plisqin-not))

  (define the-rewriter
    (/all
     ; Dont filter braced for this one:
     (/pass-ltr rest-args-pattern)
     ; OK, now do our real rewriting work
     ; Wrap these literals like (identity 'asc) so that we don't infinitely recurse
     ; (Otherwise we get asc -> (quote asc) -> (quote (quote asc)) -> ... forever)
     (/pass (make-literal-rewriter 'asc #'(identity 'asc))
            (make-literal-rewriter 'desc #'(identity 'desc))
            (make-literal-rewriter 'null #'(identity 'null))
            (make-literal-rewriter 'and #'plisqin-and)
            (make-literal-rewriter 'or #'plisqin-or)
            (make-literal-rewriter 'not #'plisqin-not)
            (make-literal-rewriter 'as #'#:as))
     (/pass valueless-dot
            dot-list
            dot-id)
     (/pass (make-infix-rewriter '(||)))
     (/pass (make-infix-rewriter '(* /)))
     (/pass (make-infix-rewriter '(+ -)))
     (/pass (make-infix-rewriter '(= < > <= >= <> like not-like)))
     (/pass (make-unary-rewriter '(plisqin-not)))
     (/pass (make-infix-rewriter '(plisqin-and)))
     (/pass (make-infix-rewriter '(plisqin-or)))
     ; Now we're done. If there are any unresolved dots, it is a syntax error.
     ; Don't filter braced for this one:
     (/pass-ltr dot-misuse)))

  (define/contract (rewrite stx)
    (-> syntax? syntax?)
    (or (the-rewriter stx) stx)))
