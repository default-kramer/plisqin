#lang racket

(provide nullchecker nulltrack<%> nulltrack?
         nullability nullability? yes no maybe
         fallback fallback? /void /minval /maxval /any fallback-symbol)

(module+ more
  ; try to use nulltrack<%> instead
  (provide gen:custom-nullability))

(require (only-in "_core.rkt" tuple? join? get-join-type)
         racket/generic)

(struct opaque-val (str)
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (let [(str (opaque-val-str me))]
       (write-string str port)))])

(define-syntax-rule (def-opaque contract-id id ...)
  (begin
    (define id (opaque-val (~a 'id)))
    ...
    (define (contract-id x)
      (or (eq? x id)
          ...))))

(def-opaque nullability? yes no maybe)
(def-opaque fallback? /void /minval /maxval /any)

; This is a quick workaround, prefer to use nulltrack<%>
(define-generics custom-nullability
  (get-custom-nullability custom-nullability))

(define nulltrack<%>
  (interface ()
    get-nullability #;(-> this nullability?)
    get-fallback    #;(-> this (or/c #f fallback?))
    ))

(define nulltrack? (is-a?/c nulltrack<%>))

(define (nullability x)
  (or (and (nulltrack? x)
           (send x get-nullability))
      (and (custom-nullability? x)
           (get-custom-nullability x))))

(define (fallback x)
  (and (nulltrack? x)
       (send x get-fallback)))

(define (fallback-symbol x)
  (if (nulltrack? x)
      (fallback-symbol (fallback x))
      (cond
        [(eq? x /void)   '/void]
        [(eq? x /minval) '/minval]
        [(eq? x /maxval) '/maxval]
        [(eq? x /any)    '/any]
        [else #f])))


;;; nullcheck-core
; Infers nullability by iterating over the arglist.
;
; If an argument's nullability is `no`, then we just move on to the next arg.
; But an argument can still be treated as `no` if it has a fallback value in
; the given list of fallbacks.
;
; The breaker is a predicate of (-> nullability? any/c).
; It allows early exit.
;
; Returns three values:
; 1. The nullability that is inferred from the arglist
; The following two values are only useful if the `breaker` returned non-false.
; 2. Contains the zero-based index that tripped the breaker.
; 3. Contains the argument that tripped the breaker.
(define (nullcheck-core arglist fallbacks breaker)
  ; start with `no` (not null) and bump up to `maybe` or `yes` as we go
  (define inferred-nullability no)
  (define arg-index 0)
  (define break-value (void))

  (for ([arg arglist])
    (define nullability
      (cond
        [(custom-nullability? arg)
         (get-custom-nullability arg)]
        [(nulltrack? arg)
         (let ([nullability (send arg get-nullability)])
           (if (eq? nullability no)
               no
               (let ([f (send arg get-fallback)])
                 (if (member f fallbacks)
                     no
                     nullability))))]
        [(tuple? arg)
         ; a tuple is never null
         no]
        [(join? arg)
         ; TODO it would probably be cleaner to make join implement nulltrack<%>
         ; ... temp workaround for now
         (let ([jt (get-join-type arg)])
           (case jt
             [(left) yes]
             [(inner) no]
             [else (begin (println (format "TODO unexpected join type: ~a" jt))
                          maybe)]))]
        [else maybe]))
    (set! inferred-nullability (cond
                                 [(eq? nullability yes) yes]
                                 [(eq? nullability maybe) maybe]
                                 [else inferred-nullability]))
    #:break (and (breaker inferred-nullability)
                 (set! break-value arg))
    (set! arg-index (add1 arg-index)))
  (values inferred-nullability arg-index break-value))

(define (make-permit-nullchecker . fallbacks)
  (lambda (arglist continuation-marks proc-name)
    (let-values ([(nullability _a _b)
                  ; breaker: we can exit as soon as we see `yes`
                  (nullcheck-core arglist fallbacks (λ (n) (eq? n yes)))])
      nullability)))

(define (make-deny-nullchecker . fallbacks)
  (lambda (arglist continuation-marks proc-name)
    (let-values ([(nullability bad-index bad-value)
                  ; breaker: we can exit (and raise an error) as soon as we have
                  ; anything other than `no` (not nullable).
                  (nullcheck-core arglist fallbacks (λ (n) (not (eq? n no))))])
      (if (eq? nullability no)
          no
          (if (empty? fallbacks)
              (err-msg-no-fallbacks
               proc-name continuation-marks
               nullability bad-index bad-value)
              (err-msg-with-fallbacks
               proc-name continuation-marks
               fallbacks nullability bad-index bad-value))))))

(define (err-msg-no-fallbacks
         proc-name continuation-marks
         nullability bad-index bad-value)
  (let ([msg (format #<<HEREDOC
~a: contract violation
  expected: a token that is non-nullable
  given: a token with nullability: ~a
  likely argument position: ~a
  possible solutions:
    If the token's true nullability is not `~a`, you may need to use `>>` to
    override the inferred nullability wherever the token is created.
  argument value: ~e
HEREDOC
                     proc-name nullability (add1 bad-index) nullability
                     "TODO-show-bad-value" #;bad-value)])
    (raise (exn:fail:contract msg continuation-marks))))

(define (err-msg-with-fallbacks
         proc-name continuation-marks
         fallbacks nullability bad-index bad-value)
  (let ([msg (format #<<HEREDOC
~a: contract violation
  expected: a token that is non-nullable or has an acceptable fallback
  given: a token with nullability: ~a
  likely argument position: ~a
  acceptable fallbacks: ~a
  possible solutions:
    If the token's true nullability is `~a`, you should use `??` to attach an
    acceptable fallback immediately before passing it into this function.
    If the token's true nullability is not `~a`, you may need to use `>>` to
    override the inferred nullability wherever the token is created.
  argument value: ~e
HEREDOC
                     proc-name nullability (add1 bad-index) fallbacks nullability nullability
                     "TODO-show-bad-value" #;bad-value)])
    (raise (exn:fail:contract msg continuation-marks))))


; Construct a procedure that can be used as a nullchecker.
; Example
#;(nullchecker
   ; Optional:
   #:accept /void /unit /any
   ; Choose one:
   #:permit-null #:deny-null
   ; Optional, TODO:
   #:over (match arglist
            [(list 'asc rest ...) rest]
            [(list 'desc rest ...) rest]
            [else arglist]))
(define-syntax (nullchecker stx)
  (syntax-case stx ()
    [(nullchecker x rest ...)
     (not (equal? (syntax-e #'x) '#:accept))
     #'(nullchecker #:accept x rest ...)]
    [(nullchecker #:accept fallback ...
                  #:permit-null)
     #'(make-permit-nullchecker fallback ...)]
    [(nullchecker #:accept fallback ...
                  #:deny-null)
     #'(make-deny-nullchecker fallback ...)]
    [else (error "nullchecker can't parse:" stx)]))
