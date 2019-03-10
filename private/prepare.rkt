#lang racket
(require "core.rkt" "to-sql.rkt" "param-binder.rkt" "dialect.rkt"
         (prefix-in db: db))

; A mutable context that holds the statement
(struct ctx (token
             renderings ; (mutable-hashof dialect (pairof sql binder))
             [current-conn #:mutable] ; the most recent connection used
             [current-binder #:mutable]
             [current-pst #:mutable])
  #:transparent)

(define (make-ctx token)
  (ctx token (make-hash) #f #f #f))

;; ctx -> dialect -> (pairof sql binder)
(define (render! ctx dialect)
  (hash-ref! (ctx-renderings ctx)
             dialect
             (λ() (parameterize ([current-dialect dialect])
                    (bind-sql (ctx-token ctx))))))

;; ctx -> db:connection -> (values db:prepared-statement binder)
; Unconditionally reprepare the ctx
(define (reprep! ctx conn)
  (define dialect (or (current-dialect)
                      (infer-dialect conn)))
  (define result (render! ctx dialect))
  (define sql (car result))
  (define binder (cdr result))
  (define pst (db:prepare conn sql))
  (set-ctx-current-conn! ctx conn)
  (set-ctx-current-binder! ctx binder)
  (set-ctx-current-pst! ctx pst)
  (values pst binder))

;; ctx? -> db:connection? -> (values db:prepared-statement? binder?)
; Conditionally reprepare the ctx
(define (prep! ctx conn)
  (-> ctx? db:connection? db:prepared-statement?)
  (if (equal? conn (ctx-current-conn ctx))
      (values (ctx-current-pst ctx) (ctx-current-binder ctx))
      (reprep! ctx conn)))

; A statement should only be creatable using (make-statement (param ...) body ...)
; Then it implements prop:procedure so that (stmt param-value ...) yields a bound-statement.
(struct statement (proc ctx)
  #:transparent
  #:property prop:procedure 0)

;; bound-statement? -> db:connection? -> db:statement-binding?
(define (do-prop:statement bs conn)
  (define ctx (bound-statement-ctx bs))
  (define bindings (bound-statement-bindings bs))
  (define-values (pst binder) (prep! ctx conn))
  (define param-values (bound-values binder bindings))
  (db:bind-prepared-statement pst param-values))

(struct bound-statement (ctx bindings)
  #:transparent
  #:property db:prop:statement do-prop:statement)

; Possibly add
;  #:pre-render '(postgres sqlite)
; to generate SQL at compile time
(define-syntax (make-statement stx)
  (syntax-case stx ()
    [(_ (param-id ...) body ...)
     (begin
       (define param-vals (map syntax-local-introduce (syntax->list #'(param-id ...))))
       #`(let* ([param-id (param #:name #'param-id)]
                ...
                [token
                 (let ([a 1])
                   body ...)]
                [ctx (make-ctx token)]
                ; Produces a bound statement
                [bind-statement
                 (λ(#,@param-vals)
                   ; make bindings, a hash of param => param-value
                   (define bindings (make-hash (map cons
                                                    (list param-id ...)
                                                    (list #,@param-vals))))
                   (bound-statement ctx bindings))])
           ; return
           (statement bind-statement ctx)))]))

(define-syntax-rule (def-statement (id param ...) body ...)
  (define id (make-statement (param ...) body ...)))


(module+ test
  (require "api.rkt" "schema.rkt" "show-table.rkt")
  (def-table Country)
  (def-fields-of Country
    CountryId CountryName CountryPopulation)

  (current-connection 'cities-example)
  (define conn (current-connection))

  (def-statement (get-country id)
    (RS from c Country
        (where (CountryId c)" = "id)))

  (db:query-rows conn (get-country 1))
  (db:query-rows conn (get-country 2))

  (db:disconnect conn))
