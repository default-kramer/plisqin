#lang racket
; Used to capture syntax objects for use in scribble documentation.

(provide doc/capture
         captured-syntaxes
         get-captured-syntax)

(define (name-extractor expr) #f)

(define (set-name-extractor! new-value)
  (set! name-extractor new-value))

(define-syntax-rule (doc/capture LITERALS PATTERN ITEM EXPR ...)
  (begin
    (define current-extractor name-extractor)
    (define/contract (new-extractor stx)
      (-> syntax? (or/c symbol? #f))
      (syntax-case stx LITERALS
        [PATTERN
         (syntax->datum #'ITEM)]
        [else (current-extractor stx)]))
    (set-name-extractor! new-extractor)
    (capture EXPR)
    ...
    (set-name-extractor! current-extractor)))

(define captured-syntaxes (make-immutable-hasheq))

(define (normalize-key key)
  (cond [(symbol? key) key]
        [(string? key) (string->symbol key)]
        [(syntax? key) (syntax->datum key)]))

(define (get-captured-syntax key)
  (hash-ref captured-syntaxes (normalize-key key)))

(define (add-expr name expr)
  (set! captured-syntaxes (hash-set captured-syntaxes name expr)))

(define-syntax-rule (capture EXPR)
  (begin
    (let ([name (name-extractor #'EXPR)])
      (if name
          (begin
            ; TODO should we track (syntax-source #'EXPR) also?
            (add-expr name #'EXPR))
          (void)))
    EXPR))