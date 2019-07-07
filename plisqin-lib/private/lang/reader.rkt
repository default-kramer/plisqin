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

; TODO experimental... not sure if this is wise yet
; A colon followed immediately by...
; 1) "string literal"      -> (#%colon "string literal")
; 2) 123 (number literal)  -> (#%colon 123)
; 3) whitespace/eof        -> :
; 4) id (a symbol)         -> :id
; 5) anything else         -> syntax error
(define (read-colon char port name line col pos)
  (define next-char (peek-char port))
  (define single-colon
    (datum->syntax #f ':
                   (list name line col pos 1)))
  (cond
    [(or (eof-object? next-char)
         (char-whitespace? next-char))
     single-colon]
    [else
     (let* ([next-stx (read-syntax name port)]
            [content (syntax-e next-stx)])
       (cond
         [(or (string? content)
              (number? content))
          (datum->syntax #f `(#%colon ,next-stx))]
         [(symbol? content)
          (datum->syntax next-stx
                         (string->symbol (format ":~a" content))
                         (list name line col pos
                               (+ 1 (syntax-span next-stx))))]
         [else
          (raise-syntax-error 'plisqin
                              "illegal use of `:`"
                              single-colon)]))]))

(define plisqin-readtable
  (make-readtable #f #\: 'non-terminating-macro read-colon))


; Returns list of syntax objects
(define (read-all source-name in)
  (define (helper source-name in accum)
    (define result (read-syntax source-name in))
    (if (not (eof-object? result))
        (helper source-name in (cons result accum))
        (reverse accum)))
  (parameterize ([current-readtable plisqin-readtable])
    (helper source-name in '())))

(define (reader source-name in)
  (define stxs (read-all source-name in))
  (define result `(module my-mod plisqin-lib/private/lang/reader
                    (require plisqin-lib/private/lang/default-require)
                    (module configure-runtime racket
                      (require (submod plisqin-lib/private/lang/reader runtime-help))
                      (lang-plisqin-setup!))
                    ,@stxs))
  (set! result (datum->syntax #f result))
  (set! result (rewriter/rewrite-dots result))
  ; strip-context per https://groups.google.com/forum/#!topic/racket-users/DFBnCqLg0Xk
  (set! result (strip-context result))
  result)

(define (read-repl source-name in)
  (define result
    (parameterize ([current-readtable plisqin-readtable]
                   [read-accept-lang #f])
      (read-syntax source-name in)))
  (if (eof-object? result)
      result
      ; TODO Not sure if strip-context is desired here...
      (rewriter/rewrite (strip-context (rewriter/rewrite-dots result)))))

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