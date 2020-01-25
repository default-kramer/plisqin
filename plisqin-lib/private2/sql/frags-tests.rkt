#lang racket

(module+ test
  (require "../_types.rkt"
           "../_dialect.rkt"
           (for-syntax racket/list)
           rackunit
           (only-in morsel-lib/sql to-sql)
           (submod "frags.rkt" strict)
           (prefix-in |.| (submod "frags.rkt" strict operators))
           (prefix-in % (submod "frags.rkt" loose))
           (prefix-in % (submod "frags.rkt" loose operators))
           (prefix-in %% (submod "frags.rkt" unsafe))
           (prefix-in %% (submod "frags.rkt" unsafe operators)))

  (define-syntax (get-dialect stx)
    (syntax-case stx ()
      [(_ #:ms) #'(mssql)]
      [(_ #:pg) #'(postgres)]
      [(_ #:lite) #'(sqlite)]))

  (define-syntax (check-single stx)
    (syntax-case stx ()
      [(_ frag kw expected)
       #`(if expected
             (let ([dialect (get-dialect kw)])
               #,(syntax/loc stx
                   (check-equal? (parameterize ([current-dialect dialect])
                                   (to-sql frag))
                                 expected
                                 (format "with dialect ~a" dialect))))
             (void))]))

  (define-syntax (check stx)
    (syntax-case stx ()
      [(_ frag args ...)
       #`(begin
           (let ([proc (λ (f #:pg [pg-expected #f]
                             #:ms [ms-expected #f]
                             #:lite [lite-expected #f]
                             #:all [all-expected #f])
                         #,(syntax/loc stx
                             (check-single f #:pg (or pg-expected all-expected)))
                         #,(syntax/loc stx
                             (check-single f #:ms (or ms-expected all-expected)))
                         #,(syntax/loc stx
                             (check-single f #:lite (or lite-expected all-expected))))])
             #,(syntax/loc stx
                 (proc frag args ...))))]))

  ; check bits and bools
  (check (select (%%bit "foo"))
         ; no conversion
         #:all "foo")
  (check (select (%%= 22 33))
         ; only MS requires Bool->Bit conversion
         #:ms "cast(case when (22 = 33) then 1 else 0 end as bit)"
         #:all "(22 = 33)")
  (check (where (%%bit "bar"))
         ; all dialects require Bit->Bool conversion
         #:all "bar = 1")
  (check (where (%%= 42 42))
         ; no conversion
         #:all "(42 = 42)"))
