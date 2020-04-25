#lang racket

(provide define-statement
         (struct-out uncompiled-statement))

(require (for-syntax syntax/strip-context
                     racket/sequence)
         "param.rkt")

(define plisqin-reserved:statement-ids '())

(define (add-statement-id id)
  (set! plisqin-reserved:statement-ids
        (cons id plisqin-reserved:statement-ids)))

(define-for-syntax (param-numbers lst)
  ; Given a list of length 4, return '(1 2 3 4)
  (sequence->list (in-range 1 (add1 (length lst)))))

(define-syntax (define-statement stx)
  ; TODO before making public:
  ; 1) use syntax-parse
  ; 2) support keywords and default values?
  ; 3) do we always need the param type?
  (syntax-case stx ()
    [(_ (id [param-id param-type] ...)
        body ...)
     (with-syntax ([the-proc (strip-context #'id)]
                   [(param-number ...)
                    (param-numbers (syntax->list #'(param-id ...)))])
       #`(begin
           (define (the-proc param-id ...)
             body ...)
           (define params (list (make-param param-number
                                            #:id 'param-id
                                            #:type param-type)
                                ...))
           (define result (apply the-proc params))
           (define id (uncompiled-statement the-proc params result))
           (module+ plisqin-reserved:statements-to-compile
             (provide plisqin-reserved:statement-ids)
             (add-statement-id 'id)
             (provide id))))]))

; proc      : the procedure that produces a token
; params    : a list of parameters
; result    : The result of `(apply proc params)`. This is usually a Token,
;             but it can be anything that `to-sql` will accept.
(struct uncompiled-statement (proc params result)
  #:property prop:procedure 0)
