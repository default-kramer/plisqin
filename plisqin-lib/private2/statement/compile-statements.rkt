#lang racket

(provide compile-statements
         statement-sql)

(require racket/struct
         (prefix-in db: db)
         "define-statement.rkt"
         "param.rkt"
         (only-in "../_dialect.rkt" current-dialect dialect?)
         (only-in "../_core.rkt" to-sql)
         (for-syntax syntax/parse
                     "define-statement.rkt"
                     racket/sequence
                     "param.rkt"
                     racket/syntax))


(define (compile us dialect)
  ; us : uncompiled-statment?
  ; returns three values
  ; 1) the compiled SQL
  ; 2) the rearrange proc, see `rewrite-params`
  ; 3) get-default-value, a proc that takes the 0-based index of an arg
  ;    and returns its default value
  (define param-list
    (uncompiled-statement-params us))
  (define token
    (uncompiled-statement-result us))
  (define orig-sql
    (parameterize ([current-dialect dialect])
      (to-sql token)))
  (define-values (sql rearrange)
    (if (need-to-rewrite? dialect)
        (rewrite-params orig-sql param-list)
        (values orig-sql identity)))

  (define param-default-values
    (list->vector (map param-default-value param-list)))
  (define (get-default-value index0)
    (vector-ref param-default-values index0))

  (values sql rearrange get-default-value))

(define (compile/raise us dialect module-name)
  (with-handlers ([exn? (lambda (ex) (raise (make-err ex us module-name)))])
    (compile us dialect)))

(define (make-err exn us module-name)
  (define msg-format #<<HEREDOC
An error occurred compiling ~a from module ~a.
The original exn is:
~a
HEREDOC
    )
  (define msg (format msg-format
                      (uncompiled-statement-name us)
                      module-name
                      exn))
  (exn:fail:sql-compilation msg
                            (exn-continuation-marks exn)
                            us exn))

(define-for-syntax (format-arglist stx)
  ; It makes our syntax templates easier to write if we use a shape that works
  ; for all arguments, and then reformat them here.
  ; This procedure reformats a syntax object like
  #;([#:keyword [arg-id has-default? default-value]]
     ...)
  ; into an argument list that `define` will accept.
  ; Note that (syntax-e has-default?) must be #t or #f.
  ; If #:keyword is (syntax #f) then it is a non-keyword argument.
  (define (handle-arg stx)
    (syntax-case stx ()
      [[arg-id has-default? default-value]
       (if (syntax-e #'has-default?)
           (syntax/loc #'arg-id
             [arg-id default-value])
           #'arg-id)]))
  (syntax-case stx ()
    [()
     (list)]
    [([keyword arg] rest ...)
     (let ([arg* (handle-arg #'arg)]
           [more (format-arglist #'(rest ...))])
       (if (syntax-e #'keyword)
           (cons #'keyword (cons arg* more))
           (cons arg* more)))]))

(define-for-syntax (get-submod modpath)
  #`(submod #,modpath plisqin-reserved:statements-to-compile))

(define-syntax (compile-one stx)
  (syntax-case stx ()
    [(_ #:id statement-id
        #:dialect dialect
        #:modpath modpath
        #:param-id [param-id ...]
        #:param-kw [param-kw ...]
        #:param-has-default [param-has-default ...])
     (with-syntax ([raw-statement-id (syntax-e #'statement-id)]
                   [(index-seq ...)
                    (sequence->list
                     (in-range 0 (length (syntax->list #'(param-id ...)))))])
       (quasisyntax/loc stx
         (begin
           (require (rename-in #,(get-submod #'modpath)
                               ; us : uncompiled-statement
                               [raw-statement-id us]))
           (define-values (sql rearrange get-default-value)
             #,(syntax/loc stx
                 ; This syntax/loc doesn't seem to have the effect I wanted, which
                 ; is that the error location would be `(compile-statements ...)`
                 (compile/raise us dialect 'modpath)))
           (define (bind-param-values
                    #,@(format-arglist #'([param-kw [param-id
                                                     param-has-default
                                                     (get-default-value index-seq)]]
                                          ...)))
             (bound-statement sql (rearrange (list param-id ...))))
           (define statement-id
             (unbound-statement bind-param-values 'statement-id sql))
           (provide statement-id)
           )))]))

(define-syntax (compile-statements stx)
  ; We could do things like to-sql and rewrite-params right now instead of in
  ; the code we are generating. In fact, I had it working that way but then
  ; got tripped up with handling the default values.
  ; Well, now that it is working it should be possible to refactor in that
  ; direction if I want to.
  ; Be careful about error messaging (not that it's perfect right now).
  ; WAIT - doing to-sql at runtime should allow the user to switch dialects
  ; based on a config setting. Is that desirable?
  (syntax-parse stx
    [(compile-statements #:module module:expr
                         #:dialect raw-dialect)
     #:declare raw-dialect (expr/c #'dialect?)
     (let* ([raise-bad-module
             (lambda ([ex #f])
               (raise-syntax-error #f "no statements found in given module"
                                   stx #'module))]
            [submod-datum (syntax->datum (get-submod #'module))]
            [statement-ids (with-handlers ([exn? raise-bad-module])
                             (dynamic-require submod-datum
                                              'plisqin-reserved:statement-ids
                                              raise-bad-module))])
       (quasisyntax/loc stx
         (begin
           (define dialect
             ; Why does this report an error location from contract.rkt?
             #,(syntax/loc #'raw-dialect raw-dialect.c))

           (unsyntax-splicing
            (for/list ([statement-id statement-ids])
              (let* ([us ; uncompiled-statement
                      (dynamic-require submod-datum statement-id)]
                     [params
                      (uncompiled-statement-params us)]
                     [param-ids
                      (map param-id params)])
                (with-syntax ([(param-id ...) param-ids]
                              [(param-kw ...)
                               (map param-kw params)]
                              [(param-has-default ...)
                               (map param-has-default? params)]
                              [id (datum->syntax stx statement-id stx)])
                  (syntax/loc stx
                    (compile-one #:id id
                                 #:dialect dialect
                                 #:modpath module
                                 #:param-id [param-id ...]
                                 #:param-kw [param-kw ...]
                                 #:param-has-default [param-has-default ...]))))
              )))))]))


(struct unbound-statement (proc id sql)
  ; A procedure that accepts the parameter values to produce a bound statement
  #:property prop:procedure 0
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (unbound-statement-printer me port #t))])

(define unbound-statement-printer
  (make-constructor-style-printer
   (lambda (us) 'unbound-statement)
   (lambda (us) (list (unbound-statement-id us)))))

(struct bound-statement (sql param-values) #:transparent
  #:property db:prop:statement
  (lambda (me conn)
    (define sql (bound-statement-sql me))
    (define param-values (bound-statement-param-values me))
    (define prepared (db:prepare conn sql))
    ; TODO saw this pattern here: https://github.com/rmculpepper/sql/blob/792895fae759c6ef60aff054c1f707bb4f15407a/private/syntax.rkt#L148
    ; Not sure if this is necessary, or just an optimization:
    (if (empty? param-values)
        prepared
        (db:bind-prepared-statement prepared param-values))))

(define/contract (statement-sql s)
  (-> (or/c unbound-statement? bound-statement?) string?)
  (cond
    [(unbound-statement? s)
     (unbound-statement-sql s)]
    [(bound-statement? s)
     (bound-statement-sql s)]))

(struct exn:fail:sql-compilation exn:fail
  (uncompiled-statement orig-exn) #:transparent)
