#lang scribble/manual
@(require scribble/core
          scribble/example
          racket
          (for-label racket))

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
          [rb (racketblock0 #,prompt #,stx)]
          [result (eval stx)])
     (if (void? result)
         (list rb)
         (list rb
               (paragraph plain (racketresult #,result))))))

@(define-syntax-rule (examples-eval eval forms ...)
   (let* ([e eval]
          [items (map (curry typeset e)
                      (syntax->list #'(forms ...)))])
     (nested-flow
      ; The SCodeFlow enables the left border that racketblock uses.
      ; The FakeExamples is for CSS: .FakeExamples > p { margin: 0 }
      (make-style "SCodeFlow FakeExamples" '())
      (list*
       (racketblock0 (code:comment "#lang plisqin"))
       (flatten items)))))

@(examples-eval
  (make-base-eval)
  (+ 2 3)
  (+ 9 9)
  (define j 3)
  (+ 10 j)
  (if (equal? j 3)
      "j is 3"
      "j is not 3"))