#lang racket

(module all racket
  (provide (all-defined-out))
  (require racket/struct)

  (define (my-custom-write x port mode)
    ; https://docs.racket-lang.org/reference/Printer_Extension.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._gen~3acustom-write%29%29
    (define (to-list x)
      (match x
        [(fragment _ kind tokens)
         (list kind (to-list tokens))]
        [(source _ alias table uid)
         `(source ,(to-list alias) ,(to-list table) ,(to-list uid))]
        [(query _ source clauses joins)
         `(query ,(to-list source) ,(to-list clauses) ,(to-list joins))]
        [(join _ type query clauses)
         `(join ,(to-list type) ,(to-list query) ,(to-list clauses))]
        [(binding _ join)
         `(binding ,(to-list join))]
        [(injection _ target placeholder fragment)
         `(injection ,(to-list target) ,(to-list placeholder) ,(to-list fragment))]
        [x #:when (list? x)
           (map to-list x)]
        [else x]))
    (match mode
      [#t (write (to-list x) port)]
      [#f (display (to-list x) port)]
      [0 (print (to-list x) port)]
      [1 (print (to-list x) port)]))

  (struct metadata (key hidden? value) #:transparent)
  (define (metadata-visible? x)
    (not (metadata-hidden? x)))

  (struct token (metadata) #:transparent) ; (listof metadata?)
  (define-syntax-rule (def-token name name? (fields ...))
    (struct name token (fields ...) #:transparent
      #:methods gen:custom-write
      [(define write-proc my-custom-write)]
      #:methods gen:equal+hash
      [(define (to-list x)
         ;(-> name? list?)
         ; Return a list containing the stuff we care about for equality.
         (match x
           [(name metadata fields ...)
            ; Put the visible metadata into a set so that the order doesn't matter
            (list (apply set (filter metadata-visible? metadata))
                  fields ...)]
           [else (error (format "wanted ~a, got ~a" 'name x))]))
       (define (equal-proc a b equal-recur)
         (and (name? a)
              (name? b)
              (equal-recur (to-list a) (to-list b))))
       (define (hash-proc x proc)
         (proc (to-list x)))
       (define hash2-proc hash-proc)]))
  (def-token source source? (alias table uid))
  (def-token fragment fragment? (kind tokens))
  (def-token query query? (source clauses joins))
  (def-token join join? (type query clauses))
  (def-token binding binding? (join))
  (def-token injection injection? (target placeholder fragment)))

; Provide everything except the constructor
(require 'all)
(define-syntax-rule (provide-struct struct-id ...)
  (begin
    (provide (except-out (struct-out struct-id) struct-id))
    ...))
(provide-struct source fragment query join binding injection token)