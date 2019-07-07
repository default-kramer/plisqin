#lang racket
(provide case-when)

(require (for-syntax syntax/parse
                     racket/list)
         "core.rkt")

(begin-for-syntax
  (define-syntax-class bare-when
    (pattern (~literal when)))
  (define-syntax-class starts-with-when
    (pattern {(~literal when) anything ...}))
  (define-syntax-class when-list
    #:description "{when x ... then y ...}"
    (pattern {(~literal when) lhs:expr (~literal then) rhs:expr}))
  (define-syntax-class else-list
    #:description "{else x}"
    (pattern {(~literal else) val:expr})))

;; Syntax -> any
; It seems that macros append paren shapes maybe?
; Specifically, in default require I have {case args ...} getting rewritten
; to {case-when args ...} but when that syntax object gets here, its
; paren-shape is (cons #\{ #\{)
; I'm guessing that (car shape) returns the "most recent" paren-shape?
; TODO figure this out and tighten up this implementation.
(define-for-syntax (paren-shape stx)
  (define shape (syntax-property stx 'paren-shape))
  (if (pair? shape)
      (car shape)
      shape))

(define-syntax (case-when stx)
  (define shape (paren-shape stx))
  ; If this macro was started with a curly brace, the syntax becomes
  ; more SQL-like, recognizing "when" and "then" literals.
  ; Otherwise it looks more like a cond.
  (cond
    [(equal? shape #\{)
     (syntax-parse stx
       ; Special error detection for {case when ...}
       [(_ x:bare-when rest ...)
        (raise-syntax-error 'case-when
                            "when forms require parentheses, e.g. {when x ... then y ...}"
                            stx #'x)]
       ; This is the correct pattern
       [(_ (~optional (~and x:expr (~not x:starts-with-when)) #:defaults ([x #'#f]))
           when:when-list ...+
           (~optional else:else-list #:defaults ([else.val #'#f])))
        #`(make-cases x else.val (list (cons when.lhs when.rhs)
                                       ...))])]
    [else
     (syntax-parse stx
       [(_ (~optional (~seq #:of of:expr)
                      #:defaults ([of #'#f]))
           [when:expr then:expr] ...+
           (~optional (~seq #:else else:expr)
                      #:defaults ([else #'#f])))
        #`(make-cases of else (list (cons when then)
                                    ...))])]))
