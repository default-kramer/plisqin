#lang racket

(provide define-statement
         (struct-out uncompiled-statement))

(require (for-syntax syntax/strip-context
                     syntax/parse
                     racket/syntax
                     racket/sequence)
         "../_types.rkt"
         "param.rkt"
         (prefix-in %% "../../unsafe.rkt"))

(define plisqin-reserved:statement-ids '())

(define (add-statement-id id)
  (set! plisqin-reserved:statement-ids
        (cons id plisqin-reserved:statement-ids)))

(define-for-syntax (param-numbers lst)
  ; Given a list of length 4, return '(1 2 3 4)
  (sequence->list (in-range 1 (add1 (length lst)))))

(define-syntax (define-statement stx)
  (define-splicing-syntax-class arg
    #:description "argument spec"
    (pattern (~seq (~optional kw:keyword)
                   (~or id:id
                        [id:id type:expr]
                        [id:id type:expr default:expr]))))
  (syntax-parse stx
    [(_ (proc-id:id a:arg ...)
        body:expr ...+)
     (with-syntax ([the-proc (strip-context #'proc-id)]
                   [(param-number ...)
                    (param-numbers (syntax->list #'(a.id ...)))]
                   [stx-proc-id
                    (generate-temporary #'proc-id)]
                   [(Type ...)
                    (generate-temporaries (syntax->list #'(a.id ...)))]
                   [(param-id ...)
                    (generate-temporaries (syntax->list #'(a.id ...)))])
       (syntax/loc stx
         (begin
           (define Type (~? a.type Scalar))
           ...
           (define param-id (make-param #:index param-number
                                        #:id (syntax a.id)
                                        #:type Type
                                        (~? (~@ #:kw (syntax a.kw)))
                                        (~? (~@ #:default a.default))))
           ...
           (define (the-proc (~@ (~? a.kw)
                                 (~? [a.id (%%val a.default)]
                                     a.id))
                             ...)
             ; We could use the Types with define/contract here.
             ; But would we even want to? I think not.
             ; There is a good chance this uncompiled proc will never get called
             ; by user code. And even if it does, the user might want to be able
             ; to pass 42 for a Number instead of (val 42), especially if they
             ; are using a non-strict dialect.
             ; Also, no contract makes testing way easier...
             body ...)
           (define result (the-proc (~@ (~? a.kw) param-id)
                                    ...))
           (define proc-id (uncompiled-statement the-proc 'proc-id (list param-id ...) result))
           (module+ plisqin-reserved:statements-to-compile
             (provide plisqin-reserved:statement-ids)
             (add-statement-id 'proc-id)
             (provide proc-id)))))]))

; proc      : the procedure that produces a token
; params    : a list of parameters
; result    : The result of `(apply proc params)`. This is usually a Token,
;             but it can be anything that `to-sql` will accept.
(struct uncompiled-statement (proc name params result)
  #:property prop:procedure 0)


(module+ test
  (require rackunit)

  (define-statement (f1 a)
    (list a))
  (check-equal? (f1 'a)
                '(a))

  (define-statement (f2 a
                        [b Number])
    (list a b))
  (check-equal? (f2 'a 'b)
                '(a b))

  (define-statement (f3 a
                        [b Number]
                        [c String "c-default"])
    (list a b c))
  (check-equal? (f3 'a 'b)
                (list 'a 'b (%%val "c-default")))
  (check-equal? (f3 'a 'b 'c)
                '(a b c))

  (define-statement (f4 a
                        [b Number]
                        [c String "c-default"]
                        #:d d
                        #:e [e Number]
                        #:f [f String "f-default"])
    (list a b c d e f))
  (check-equal? (f4 'a 'b #:d 'd #:e 'e)
                (list 'a 'b (%%val "c-default") 'd 'e (%%val "f-default")))
  (check-equal? (f4 1 2 3 #:d 4 #:e 5 #:f 6)
                '(1 2 3 4 5 6))
  )
