#lang racket
(module+ test
  (require rackunit))
(require "core.rkt")

(provide from join inject)

(define/contract (handle-from x alias)
  (-> queryable? string? source?)
  (cond
    [(source? x) x]
    [(string? x) (source alias x)]
    [(query? x) (s:query-source x)]
    [(join? x) (handle-from (s:join-query x) alias)]
    [(subquery? x) (source alias x)]
    [(procedure? x) (x alias)]))

(define/contract (create-query x src)
  (-> any/c source? query?)
  (cond
    [(join? x) (struct-copy s:query (convert-to-subquery x)
                            [source src])]
    [(query? x) (struct-copy s:query x
                             [source src])]
    [else (make-query src)]))

(define/contract (create-join type x src)
  (-> join-type? any/c source? join?)
  (cond
    [(or (query? x)
         (join? x))
     (make-join type x)]
    [else (make-join type src)]))

(define-syntax-rule (from srcvar MAYBE-MACRO statements ...)
  ; OUCH - we have to immediately bind MAYBE-MACRO or it can get evaluated repeatedly
  ; and we wind up with a bunch of different UIDs! Why is this??
  (query-scope
   (let* ([src MAYBE-MACRO]
          [srcvar (handle-from src (format "~a" (syntax->datum #'srcvar)))]
          [q (create-query src srcvar)])
     (apply-all q statements ...))))

(define-syntax-rule (join srcvar MAYBE-MACRO statements ...)
  (query-scope
   (let* ([src MAYBE-MACRO]
          [srcvar (handle-from src (format "~a" (syntax->datum #'srcvar)))]
          [j (create-join 'InnerJoin src srcvar)])
     (apply-all j statements ...))))

(define-syntax apply-all
  (syntax-rules (join join-attach define)
    [(_ q) q]
    [(_ q (define (f args ...) body ...) statements ...)
     (let ([f (lambda (args ...) body ...)])
       (apply-all q statements ...))]
    [(_ q (define x y) statements ...)
     (let ([x y])
       (apply-all q statements ...))]
    [(_ q (join-attach j joinstuff ...) statements ...)
     (let* ([j (join j joinstuff ...)]
            [q (add-join q j)]
            [j (binding j)])
       (apply-all q statements ...))]
    [(_ q (join j joinstuff ...) statements ...)
     (apply-all q
                (join-attach j joinstuff ...)
                statements ...)]
    [(_ q statement statements ...)
     (let ([q (add-statement q statement)])
       (apply-all q statements ...))]))

(define (make-injection-body target tokens)
  (if (and (= 1 (length tokens))
           (procedure? (car tokens)))
      ; given 1 token that is a procedure - apply it to the source
      ((car tokens) target)
      ; else just build a scalar
      (scalar tokens)))

(define (get-target target)
  (cond
    ; Injections were working just fine when bindings didn't exist and we just
    ; used raw sources. So what happens if we use the raw source as the injection
    ; target? It should work, right?
    ;[(binding? target) (get-src target)]
    ; HA HA let's go!
    [else target]))

(define-syntax inject
  (syntax-rules ()
    [(inject [VAR TARGET] TOKENS ...)
     (let* ([target (get-target TARGET)]
            [VAR (make-injection-placeholder target)])
       (injection target VAR (make-injection-body VAR (list TOKENS ...))))]
    [(inject VAR TOKENS ...)
     (inject [VAR VAR] TOKENS ...)]))

(module+ test
  ; simple test of referential transparency
  (check-equal?
   (from x "X" (join y "Y" (join-on y".YID = "x".YID")))
   (from x "X" (join y "Y" (join-on y".YID = "x".YID")))))