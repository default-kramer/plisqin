#lang racket

(require plisqin
         "helpers.rkt")

(define-syntax-rule (make Type [content ...])
  (let ([token (%%scalar content ...)])
    (>> token #:cast Type)))

(check-sql (from a 'A
                 (select (count a)))
           #:all "select count(*) from A a")

(check-sql (from a 'A
                 (select (count 'distinct (make Number? {a".Foo"}))))
           #:all "select count(distinct a.Foo) from A a")

(check-sql (from a 'A
                 (select (count (make Number? {a".Foo"}))))
           #:all "select count(a.Foo) from A a")

(check-sql (from a 'A
                 (when #f
                   (error "nope"))
                 (join b 'B
                       (when #f
                         (error "nope2"))))
           #:all "select a.* from A a inner join B b on 1=1")
