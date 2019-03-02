#lang racket
(provide case-when)

(require (for-syntax syntax/parse)
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

(define-syntax (case-when stx)
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
                                    ...))]))
