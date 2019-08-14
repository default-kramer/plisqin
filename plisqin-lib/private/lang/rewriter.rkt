#lang racket
(require (for-syntax racket
                     "rewrite-lib.rkt"
                     syntax/readerr)
         "rewrite-lib.rkt"
         syntax/readerr)
(provide (for-syntax rewrite rewrite-dots)
         rewrite rewrite-dots
         stx-prop-dot-type)

; Duplicate the definitions into begin-for-syntax.
(define-syntax-rule (define-everywhere forms ...)
  (begin
    (begin-for-syntax forms ...)
    forms ...))

(define-everywhere
  ; This property exists for historical reasons, when the readtable did something with dots.
  ; That wasn't the right approach, so this property could probably be refactored away now.
  (define stx-prop-dot-type 'plisqin-dot-type)

  ;; Syntax -> Any
  (define (chained? stx)
    (eq? 'chained
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

  ; TODO - this rule is flimsy and I should probably get rid of it.
  ; The problem is that if you have
  #;{foo (.equal? 9)}
  ; The dot won't get rewritten because it is in (regular parens).
  ; (We could add a special case for that I suppose...)
  ; But if you change it to braces, like this:
  #;{foo {.equal? 9}}
  ; Now you might trigger more rewrite rules in the inner expression.
  ; Messy...
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
                                                  expr) ; drop the props (paren-shape) to avoid further rewriting!
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

  ; Splits "foo.bar.baz" into '("foo" "." "bar" "." "baz")
  ; Tests at end of file
  (define (custom-split str)
    (cond
      [(equal? str "")
       (list)]
      [(equal? #\. (string-ref str 0))
       (cons "." (custom-split (substring str 1)))]
      [else
       (let* ([word (car (regexp-match #rx"[^\\.]+" str))]
              [rest (substring str (string-length word))])
         (cons word (custom-split rest)))]))

  ; Takes a list of split strings like '("foo" "." "bar")
  ; and produces a list of syntax objects.
  ; We keep the dot here; a later step will rewrite it.
  (define (convert-strs stx strs offset)
    ;(-> syntax? (listof string?) int? (listof syntax?))
    (if (empty? strs)
        '()
        (let* ([str (car strs)]
               [length (string-length str)]
               [new-offset (+ offset length)]
               [datum (or (string->number str)
                          (string->symbol str))]
               [result
                (datum->syntax stx datum
                               (list (syntax-source stx)
                                     (syntax-line stx)
                                     (+ offset (syntax-column stx))
                                     (+ offset (syntax-position stx))
                                     length)
                               stx)]
               ; Set the "chained" property for historical purposes.
               ; (This could probably be done better.)
               [result (if (equal? str ".")
                           (syntax-property result stx-prop-dot-type 'chained)
                           result)])
          (cons result (convert-strs stx
                                     (cdr strs)
                                     new-offset)))))

  ; If stx is an identifier that can be split, return the list of syntax
  ; objects to splice in its place. Otherwise return #f.
  (define (split-dots stx)
    ;(-> syntax? (or/c #f (listof syntax?)))
    (define strs
      (when (identifier? stx)
        (let ([str (format "~a" (syntax-e stx))])
          (when (not (string-contains? str ".."))
            (custom-split str)))))
    (define stxs
      (when (and (not (void? strs))
                 ((length strs) . >= . 2))
        (convert-strs stx strs 0)))
    (if (not (void? stxs))
        stxs
        #f))

  (define (do-dots stx)
    (syntax-case stx ()
      [{a.b rest ...}
       (if (braced? stx)
           (let* ([lst (syntax->list stx)]
                  [a.b (car lst)]
                  [rest (cdr lst)]
                  [split (split-dots a.b)])
             (if split
                 (datum->syntax stx
                                (append split rest)
                                stx stx)
                 #f))
           #f)]
      [else #f]))

  (require (prefix-in plisqin- (only-in "../operators.rkt"
                                        and
                                        or
                                        not)))

  (define the-rewriter
    (/all
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
     (/pass (make-infix-rewriter '(??)))
     (/pass (make-infix-rewriter '(= < > <= >= <> like not-like is is-not in not-in)))
     (/pass (make-unary-rewriter '(plisqin-not)))
     (/pass (make-infix-rewriter '(plisqin-and)))
     (/pass (make-infix-rewriter '(plisqin-or)))))

  (define/contract (rewrite stx)
    (-> syntax? syntax?)
    (or (the-rewriter stx) stx))

  (define dot-rw (/pass-ltr do-dots))
  (define/contract (rewrite-dots stx)
    (-> syntax? syntax?)
    (or (dot-rw stx) stx)))

(module+ test
  (require rackunit)
  (check-equal? (custom-split ".foo.bar.baz.")
                '("." "foo" "." "bar" "." "baz" "."))
  (check-equal? (custom-split "foo.bar.baz.")
                '("foo" "." "bar" "." "baz" "."))
  (check-equal? (custom-split ".foo.bar.baz")
                '("." "foo" "." "bar" "." "baz")))
