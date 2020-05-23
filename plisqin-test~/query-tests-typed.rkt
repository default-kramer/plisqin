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

(check-sql (from a 'A
                 (limit 5))
           #:ms "select top 5 a.* from A a"
           #:pg "select a.* from A a limit 5"
           #:lite "select a.* from A a limit 5")

(check-sql (from a 'A
                 (limit 5)
                 (limit #f))
           #:all "select a.* from A a")

(check-sql (from a 'A
                 (%%order-by a".blah")
                 (offset 2))
           #:ms "select a.* from A a order by a.blah offset 2 rows"
           #:pg "select a.* from A a order by a.blah offset 2"
           #:lite "select a.* from A a order by a.blah offset 2")

(check-sql (from a 'A
                 (offset 2)
                 (offset #f))
           #:all "select a.* from A a")

(check-sql (from a 'A
                 (distinct #t))
           #:all "select distinct a.* from A a")

(check-sql (from a 'A
                 (distinct #t)
                 (distinct #f))
           #:all "select a.* from A a")
