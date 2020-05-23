#lang racket

(require plisqin
         rackunit)

(define this-file
  "error-tests.rkt")

(define (force x)
  ; needed because queries and joins are evaluated lazily
  (let ([tmp (~a x)])
    x))

(define-syntax (check-exn+message stx)
  ; Check that an exception is thrown and that each snippet occurs in the message
  (syntax-case stx ()
    [(_ expr snippets ...)
     #`(begin
         #,(syntax/loc stx
             (check-exn exn? (lambda () (force expr))))
         (with-handlers ([exn? (lambda (exn)
                                 (let ([message (exn-message exn)])
                                   #,@(for/list ([snippet (syntax->list #'(snippets ...))])
                                        (quasisyntax/loc snippet
                                          (check-true (string-contains? message #,snippet)
                                                      (format "Missing: ~a   ***   From: ~a"
                                                              #,snippet message))))))])
           (force expr)))]))

(check-exn+message (join-on (%%sql "not a bool"))
                   "join-on: contract violation"
                   "expected an argument list satisfying one of the following:"
                   "Boolish? -> JoinOn?"
                   "given an argument list with the following types:"
                   "(Token?)")

(check-exn+message (coalesce "not a Token?")
                   "coalesce: contract violation"
                   "expected an argument list satisfying one of the following:"
                   "String? String? ...+ -> String?"
                   "Datetime? Datetime? ...+ -> Datetime?"
                   "Number? Number? ...+ -> Number?"
                   "given an argument list with the following types:"
                   "(Untyped)")

(define/contract (test-instanceof-symbol x)
  (-> (instanceof 'Foo) any/c)
  (%%where x".whatever = 0"))

(check-exn+message (from x 'Bar
                         (test-instanceof-symbol x))
                   "expected: (instanceof (quote Foo))"
                   "in: the 1st argument of"
                   "(-> (instanceof 'Foo) any/c)"
                   "given: #<tuple: 'Bar>"
                   "blaming:" this-file)

; regression test - this should not throw an exception
(check-not-exn
 (lambda () (force (from x 'Foo
                         (test-instanceof-symbol x)))))

; Violate the clause contract
(check-exn+message (from x 'Foo
                         '(not a query clause))
                   "from: contract violation"
                   "expected: (or/c void? QueryClause? (listof (or/c void? QueryClause?)))"
                   "given: '(not a query clause)"
                   "blaming:" this-file)
(check-exn+message (from x 'X
                         (join y 'Y
                               '(not a join clause)))
                   "join: contract violation"
                   "expected: (or/c void? JoinClause? (listof (or/c void? JoinClause?)))"
                   "given: '(not a join clause)"
                   "blaming:" this-file)
(check-exn+message (from x 'X
                         (define y
                           (join y 'Y #:to x
                                 '(not a join clause)))
                         (%%select y".blah"))
                   "join: contract violation"
                   "expected: (or/c void? JoinClause? (listof (or/c void? JoinClause?)))"
                   "given: '(not a join clause)"
                   "blaming:" this-file)

; Violate the queryable contract
(check-exn+message (from x 123)
                   "from: contract violation"
                   "expected: from-queryable?"
                   "given: 123"
                   "blaming:" this-file)
(check-exn+message (from x 'X
                         (join y 234))
                   "join: contract violation"
                   "expected: join-queryable?"
                   "given: 234"
                   "blaming:" this-file)
(check-exn+message (from x 'X
                         (define y
                           (join y 345 #:to x)))
                   "join: contract violation"
                   "expected: join-queryable?"
                   "given: 345"
                   "blaming:" this-file)

; Violate the "join to" contract
(check-exn+message (join x 'X
                         #:to '(not an instance))
                   "join: contract violation"
                   "expected: instance?"
                   "given: '(not an instance)"
                   "blaming:" this-file)

; Regression - our type machinery couldn't handle a raw symbol as a contract.
; Wrap the symbol in or/c as a cheap workaround.
(check-exn+message (from a 'A
                         (select (count 'distinct a)))
                   "count: contract violation")


; Comparison operators require non-null or an acceptable fallback
(check-exn+message (from a 'A
                         (where (.< (>> (%%sql a".foo") #:cast Number?)
                                    (>> (%%sql a".bar") #:cast Number?))))
                   "expected: a token that is non-nullable or has an acceptable fallback"
                   "given: a token with nullability: maybe"
                   "argument value: (sql #<tuple: 'A> \".foo\")")

; Where requires non-null, no fallbacks accepted
(check-exn+message (from a 'A
                         (where (>> (%%sql a".foo") #:cast Bool?)))
                   "expected: a token that is non-nullable"
                   "given: a token with nullability: maybe"
                   "argument value: (sql #<tuple: 'A> \".foo\")")
