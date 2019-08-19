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
    [(raw-sql? x) (handle-from (raw-sql-content x) alias)]
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

(define (get-join-type x)
  #;(-> any/c join-type?)
  (cond
    [(attached-join? x)
     (get-join-type (attached-join-join x))]
    [(join? x)
     (s:join-type x)]
    [else 'infer-join-type]))

(define-syntax-rule (join srcvar MAYBE-MACRO statements ...)
  (query-scope
   (let* ([src MAYBE-MACRO]
          [srcvar (handle-from src (format "~a" (syntax->datum #'srcvar)))]
          [join-type (get-join-type src)]
          [j (create-join join-type src srcvar)])
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

(define-syntax inject
  (syntax-rules ()
    [(inject [VAR TARGET] TOKENS ...)
     (let* ([target TARGET]
            [VAR (make-injection-placeholder target)])
       (injection target VAR (make-injection-body VAR (list TOKENS ...))))]
    [(inject VAR TOKENS ...)
     (inject [VAR VAR] TOKENS ...)]))

(module+ test
  ; simple test of referential transparency
  (check-equal?
   (RS from x "X" (join y "Y" (join-on y".YID = "x".YID")))
   (RS from x "X" (join y "Y" (join-on y".YID = "x".YID"))))

  ; check that the join-type is preserved when appending
  (check-equal?
   (join j (join j "J" 'cross-apply))
   (join j "J" 'cross-apply))
  ; default join type is 'infer-join-type
  (check-equal?
   (join j "J")
   (join j "J" 'infer-join-type)))
