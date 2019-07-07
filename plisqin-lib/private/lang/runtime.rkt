#lang racket
(require (for-syntax "rewrite-lib.rkt")
         (prefix-in racket/ racket))
(provide (rename-out [-app #%app]
                     [-do-dot #%do-dot]))

(define-syntax (-app stx)
  (copy-srcloc
   stx
   (syntax-case stx ()
     [{_ val}
      (if (braced? stx)
          #'(app-or-return val)
          #'(racket/#%app val))]
     [(_ stuff ...)
      #'(racket/#%app stuff ...)])))

(define-syntax-rule (app-or-return val)
  (let ([x val])
    (if (procedure? x)
        (x)
        x)))

(define-syntax (-do-dot stx)
  (syntax-case stx ()
    [(_ val func args ...)
     #`(let ([v val]
             [f func])
         (if (procedure? v)
             (raise-syntax-error #f "left side of dot cannot be a procedure" #'val)
             #,(copy-srcloc
                stx
                #'(f v args ...))))]))