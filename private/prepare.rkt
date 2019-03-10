#lang racket

(provide make-statement
         def-statement)

(require "core.rkt" "to-sql.rkt" "param-binder.rkt" "dialect.rkt"
         (prefix-in db: db)
         (for-syntax syntax/parse))

; A mutable context that holds the statement.
; When invoked with a connection that differs from "current-conn", we
; prepare it and store the results into the mutable fields.
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
  (syntax-parse stx
    [(_ (param-id:id ...) body:expr ...+)
     (begin
       (define param-vals (map syntax-local-introduce (syntax->list #'(param-id ...))))
       #`(let* ([param-id (param #:name #'param-id)]
                ...
                ; Use `let` instead of `begin` here so that `body ...` can contain `define`
                [token (let () body ...)]
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

(define-syntax (def-statement stx)
  (syntax-parse stx
    [(_ (proc-id:id param-id:id ...) body:expr ...+)
     #'(define proc-id
         (make-statement (param-id ...) body ...))]))

(module+ test
  (require "api.rkt" "schema.rkt" "show-table.rkt"
           rackunit)
  (def-table Country)
  (def-fields-of Country
    CountryId CountryName CountryPopulation)

  (current-connection 'cities-example)
  (define conn (current-connection))

  (def-statement (get-country id)
    (define testdef "blah")
    (void testdef)
    (RS from c Country
        (where (CountryId c)" = "id)))

  (define rows
    (db:query-rows conn (get-country 1)))
  (check-equal? (length rows) 1)

  (set! rows
        (db:query-rows conn (get-country -1)))
  (check-equal? (length rows) 0)

  ; Note - there is an opportunity for
  #;((get-country 1) #:conn 'default)
  ; to mean "execute the bound-statement using the current connection."
  ; And the #:conn parameter is optional of course.
  ; The ctx would need to hold a callback representing which of query-rows, query-exec, etc... you want to do

  (db:disconnect conn))
