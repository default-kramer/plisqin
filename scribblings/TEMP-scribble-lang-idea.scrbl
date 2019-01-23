#lang scribble/manual
@(require scribble/example
          (for-label racket)
          (for-syntax racket scribble/example))

@(examples
  (define j 3)
  (+ 2 j)
  (* 10 j))

My hack:

@(begin-for-syntax
   ; Tweaks a syntax object's srcloc to the given line and column,
   ; at least as far as Scribble is concerned.
   ; Wait a minute, don't I already have this? Like (move #'here stx) maybe?
   (define (realign LINE COL stx)
     (define kids (syntax->list stx))
     (define result
       (datum->syntax stx
                      (if kids
                          (map (curry realign LINE #f) kids)
                          (syntax->datum stx))
                      (list (syntax-source stx)
                            LINE
                            (or COL (syntax-column stx))
                            (syntax-position stx)
                            (syntax-span stx))
                      stx
                      stx))
     ;(println result)
     result)

   ; fool-examples
   ; Given a list of syntax objects such as
   #;(stx-list (+ 1 2)
               (* 2 3))
   ; This returns a list of syntax objects such as
   #;(stx-list > (+ 1 2)
               3
               > (* 2 3)
               6)
   (define (fool-examples stxs accum curr-line evaluator)
     (match stxs
       [(list) accum]
       [(list stx rest ...)
        ; append the prompt ">" to the current line
        (set! accum (cons (realign curr-line 1 (datum->syntax #f '>)) accum))
        ; append the syntax object to the current line
        (set! accum (cons (realign curr-line 3 stx) accum))
        ; advance line
        (set! curr-line (add1 curr-line))
        ; evaluate result, maybe put it on the current line
        ; NOTE - this is where I can translate stx from #lang plisqin to racket
        (define result (evaluator stx))
        (when (not (void? result))
          (set! accum (cons (realign curr-line 1
                                     ; Use unsyntax in the current context so that racketblock
                                     ; will recognize it and use this racketresult:
                                     (datum->syntax #'HERE `(unsyntax (racketresult ,result))))
                            accum))
          (set! curr-line (add1 curr-line)))
        ; recurse
        (fool-examples rest accum curr-line evaluator)])))

@(define-syntax (langset stx)
   (syntax-case stx ()
     [(langset form ...)
      (begin
        (define result #`(racketblock
                          (code:comment "#lang plisqin")))
        (define curr-line (+ 2 (syntax-line result)))
        (define lines (fool-examples (syntax->list #'(form ...))
                                     '()
                                     curr-line
                                     (make-base-eval)))
        (datum->syntax result
                       (append (syntax->list result)
                               (reverse lines))
                       result
                       result))]))

@(langset
  (define j 3)
  (+ 2 j)
  (* 10 j))