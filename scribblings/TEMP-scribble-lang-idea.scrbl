#lang scribble/manual
@(require scribble/core
          scribble/example
          scribble/html-properties
          racket
          (for-label racket))

@(define PRes
   (make-style "PRes noselect"
               (list (make-css-addition "PStyles.css"))))
@(define PlisqinExamples
   (make-style "PExamples if-plisqin SCodeFlow"
               (list (make-css-addition "PStyles.css")
                     (make-js-addition "PScript.js"))))
@(define RacketExamples
   (make-style "PExamples if-racket SCodeFlow"
               (list (make-css-addition "PStyles.css")
                     (make-js-addition "PScript.js"))))

@(define/contract (typeset eval stx)
   (-> any/c syntax? (listof block?))
   (let* ([col (syntax-column stx)]
          ; If stx is "(+ a b)" subtracting 2 from the column should give us
          ;         "> (+ a b)"
          [col (cond
                 [(false? col) #f]
                 [(col . >= . 2) (col . - . 2)]
                 [else 0])]
          [prompt (datum->syntax #f '>
                                 (list (syntax-source stx)
                                       (syntax-line stx)
                                       col
                                       #f
                                       #f))]
          ; Actually the prompt is just clutter I think...
          ;[rb (racketblock0 #,prompt #,stx)]
          [rb (racketblock0 #,stx)]
          [result (eval stx)])
     (if (void? result)
         (list rb)
         (list rb
               (paragraph PRes (racketresult #,result))))))

@(define-syntax-rule (examples-eval eval style forms ...)
   (let* ([e eval]
          [items (map (curry typeset e)
                      (syntax->list #'(forms ...)))])
     (nested-flow style (flatten items))))

@(define-syntax-rule (plisqin-eval forms ...)
   (examples-eval (make-base-eval) PlisqinExamples forms ...))

@(define-syntax-rule (racket-eval forms ...)
   (examples-eval (make-base-eval) RacketExamples forms ...))

@(define-syntax-rule
   (dual-lang (plisqin ...) (racket ...))
   (table
    (make-style "PDualLang" '())
    (list (list (paragraph (make-style "PLangToggle" '()) ""))
          (list (plisqin-eval plisqin ...))
          (list (racket-eval racket ...)))))

Blah blah blah, here's some code:
@(dual-lang
  [(+ 3 4)]
  [(* 8 8)])

Look! More code:
@(dual-lang
  [{define j 3}
   (if {equal? j 3}
       "j is 3"
       "j is not 3")]
  [(define j 3)
   (if (equal? j 3)
       "j is 3"
       "j is not 3")])