#lang racket

#;(provide to-sql bind-sql rewrite)

(module+ test
  (require rackunit)
  (check-equal? #f "TODO revisit these tests and exports"))

#;(module+ test
    (require (prefix-in op: "operators.rkt"))
    (check-sql
     (from a "A")
     "select a.* from A a")
  
    (check-sql
     (RS from t "Title"
         (join c "Credit"
               (join-on c".TitleID = "t".TitleID"))
         (select t".X")
         (select c".Y"))
     #<<HEREDOC
select
  t.X
  , c.Y
from Title t
inner join Credit c
   on c.TitleID = t.TitleID
HEREDOC
     )

    ; same as above, let's just inline it
    (check-sql
     (RS from t "Title"
         (select t".X")
         (select (join c "Credit"
                       (join-on c".TitleID = "t".TitleID"))
                 ".Y"))
     #<<HEREDOC
select
  t.X
  , c.Y
from Title t
inner join Credit c
   on c.TitleID = t.TitleID
HEREDOC
     )

    (check-sql
     (RS from x "X"
         (join y "Y" 'left-join
               (join-on "(1 = 1)"))
         (select 1))
     #<<HEREDOC
select 1
from X x
left join Y y
on (1 = 1)
HEREDOC
     )

    ; TODO should we auto-convert join-on clauses to where clauses if the join type is an apply?
    (check-sql
     (RS from x "X"
         (join y "Y" 'cross-apply
               (where y".XID = "x".XID"))
         (select y".BLAH"))
     #<<HEREDOC
select y.BLAH
from X x
cross apply (
    select y.*
    from Y y
    where y.XID = x.XID) y
HEREDOC
     )

    (check-sql
     (RS from x (subquery "select 1 as ONE")
         (select x".ONE"))
     #<<HEREDOC
select x.ONE
from (select 1 as ONE) x
HEREDOC
     )
    (check-sql
     (RS from x (subquery (from y "Y"
                                (select y".ONE")))
         (select x".ONE"))
     #<<HEREDOC
select x.ONE
from (
    select y.ONE from Y y) x
HEREDOC
     )

    ; check that infer-join-type is working
    (check-sql
     (RS from x "X"
         (define y
           (join y "Y"
                 'left-join
                 (join-on y".foo = "x".foo")))
         ; z is inferred to be a left join because it depends on y (which is left)
         (join z "Z"
               (join-on z".bar = "y".bar"))
         (select z".baz"))
     #<<HEREDOC
select z.baz
from X x
left join Y y on y.foo = x.foo
left join Z z on z.bar = y.bar
HEREDOC
     )

    ; check as with raw-sql
    (check-sql
     (RS from x "X"
         (select x".foo" #:as "bar"))
     "select x.foo as bar from X x")

    (define-syntax-rule (with-MS forms ...)
      (parameterize ([current-dialect (mssql)])
        forms ...))
    (define-syntax-rule (with-PG forms ...)
      (parameterize ([current-dialect (postgres)])
        forms ...))

    (define frag (op:+ (RS sql "getdate()")
                       (interval 3 :hours)
                       (interval 1 :day)))
    (with-MS
        (check-equal?
         (to-sql frag)
         "dateadd(day, 1, dateadd(hour, 3, getdate()))"))
    (with-PG
        (check-equal?
         (to-sql frag)
         "(getdate() + interval '3 hour' + interval '1 day')"))
    (set! frag (op:- (RS sql "getdate()")
                     (interval 3 :hours)
                     (interval 1 :day)))
    (with-MS
        (check-equal?
         (to-sql frag)
         "dateadd(day, -1, dateadd(hour, -3, getdate()))"))
    (with-PG
        (check-equal?
         (to-sql frag)
         "(getdate() + interval '-3 hour' + interval '-1 day')"))

    (void "end test submodule"))
