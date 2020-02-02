#lang racket

(require (only-in plisqin from join to-sql)
         plisqin-lib/unsafe
         rackunit)

(define-syntax (check-sql stx)
  (syntax-case stx ()
    [(_ q expected)
     (syntax/loc stx
       (check-equal? (to-sql q) expected))]))

(define-syntax-rule (test body ...)
  (let ()
    body ...))

(test
 ; Aliases are made to be unique if needed.
 ; Both of these want the alias "x" but the subquery gets renamed to "x1":
 (define (my-subquery parent)
   (from x "Sub"
         (where x".Something = "parent".Something")))
 (define my-query
   (from x "Parent"
         (where "not "(exists (my-subquery x)))))

 (check-sql my-query #<<HEREDOC
select x.*
from Parent x
where not exists (
  select x1.*
  from Sub x1
  where x1.Something = x.Something
)
HEREDOC
            ))


(test
 ; Joins are included even if they don't appear in a fragment:
 (define my-query
   (from x "X"
         (join y "Y"
               (join-on "1=1"))))

 (check-sql my-query #<<HEREDOC
select x.*
from X x
inner join Y y
   on 1=1
HEREDOC
            ))

(fail "TODO make from/join accept lists of clauses")
#;(test
   ; The query-building macros accept a single clause or a list for greater composability:
   (define my-query
     (from x "X"
           (if #t
               (select x".First")
               (list))
           (if #t
               (list
                (select x".Second")
                (select x".Third"))
               (list))
           (if #f
               (select x".Fourth")
               (list))))
   (check-sql my-query
              #<<HEREDOC
select
  x.First
  , x.Second
  , x.Third
from X x
HEREDOC
              ))

(test
 ; Aggregate functions don't need to be injected if they refer only to the main query and "simple" joins:
 (define my-query
   (from t "Title"
         (join r "Rating"
               (join-on r".TitleID = "t".TitleID"))
         (select (sum t".TitleID + "r".Score"))
         (group-by t".TitleID")))
 (check-sql my-query #<<HEREDOC
select sum(t.TitleID + r.Score)
from Title t
inner join Rating r
   on r.TitleID = t.TitleID
group by t.TitleID
HEREDOC
            ))

(test
 ; The SQL `having' clause:
 (define my-query
   (from r "Rating"
         (group-by r".TitleID")
         (having "count("r".TitleID) > 100")
         (select r".TitleID")
         (select (avg r".Score"))))
 (check-sql my-query #<<HEREDOC
select r.TitleID
  , avg(r.Score)
from Rating r
group by r.TitleID
having count(r.TitleID) > 100
HEREDOC
            ))

(test
 ; We can deduplicate injections!
 (define my-query
   (from t "Title"
         (join ratings "Rating"
               (group-by ratings".TitleID")
               (join-on (scalar ratings".TitleID")" = "t".TitleID"))
         (select (avg ratings".Score")" as AvgScore")
         (select (avg ratings".Score")" as AvgScore2")))
 (check-sql my-query #<<HEREDOC
select ratings.__INJECT1 as AvgScore
  , ratings.__INJECT1 as AvgScore2
from Title t
inner join (
  select ratings.TitleID as __INJECT0
    , avg(ratings.Score) as __INJECT1
  from Rating ratings
  group by ratings.TitleID
) ratings
   on ratings.__INJECT0 = t.TitleID
HEREDOC
            ))

(test
 ; The old resolve-joins was designed on the assumption that we would not find the source of
 ; a join in the fragments. In other words, that all joins would be inlined.
 ; This is no longer true. Let's try to create a failing test here:
 (define (TitleType-of/s title)
   (join tt "TitleType" #:to title
         (join-on tt".TitleTypeID = "title".TitleTypeID")))

 (define my-query
   (from t "Title"
         (join tt1 (TitleType-of/s t))
         (join tt2 (TitleType-of/s t))
         (select tt1".a1")
         (select tt2".a2")
         (select (TitleType-of/s t)".a3")
         (select (TitleType-of/s t)".a4")))

 (check-sql my-query #<<HEREDOC
select tt.a1
  , tt.a2
  , tt.a3
  , tt.a4
from Title t
inner join TitleType tt
   on tt.TitleTypeID = t.TitleTypeID
HEREDOC
            ))

(test
 ; Regression test. A joined subquery should be able to explicitly join other other joins.
 (define my-query
   (from x "X"
         (join y "Y"
               (join-on "'y'='y'")
               (join z "Z"
                     (join-on "'z'='z'"))
               (select y".Foo")
               (select z".Bar"))))
 (check-sql my-query #<<HEREDOC
select x.*
from X x
inner join (
  select y.Foo
    , z.Bar
  from Y y
  inner join Z z
     on 'z'='z'
) y
   on 'y'='y'
HEREDOC
            ))

(fail "TODO fix this test - `(count y)` should mean count(*)")
#;(test
   ; Auto-inject scalars in join-on clauses:
   (define my-query
     (from x "X"
           (join y "Y"
                 (group-by y".XID")
                 (join-on (scalar y".XID")" = "(scalar x".XID")))
           (select x".*")
           (select (count y)" as NumYs")))
   (check-sql my-query #<<HEREDOC
select x.*
  , y.__INJECT1 as NumYs
from X x
inner join (
  select y.XID as __INJECT0
    , count(*) as __INJECT1
  from Y y
  group by y.XID
) y
   on y.__INJECT0 = x.XID
HEREDOC
              ))


; Tests of `exists`
(check-sql (from x "X"
                 (where (exists "select * from blah")))
           #<<HEREDOC
select x.*
from X x
where exists (select * from blah)
HEREDOC
           )

(check-sql (from x "X"
                 (where (exists (from y "Y"
                                      (where y".Foo = "x".Bar")))))
           #<<HEREDOC
select x.*
from X x
where exists (
  select y.*
  from Y y
  where y.Foo = x.Bar
)
HEREDOC
           )

; Do we still want to support this?
(fail "TODO fix this test (needs join->query conversion)")
#;(check-sql (from x "X"
                   (where (exists (join y "Y" #:to x
                                        (join-on y".Foo = "x".Bar")))))
             #<<HEREDOC
select x.*
from X x
where exists (
  select y.*
  from Y y
  where y.Foo = x.Bar
)
HEREDOC
             )


(fail "TODO bring these `count` tests back")
(void
 #<<OLD_SCRIBBLE_CODE

@(test)
Tests of @(racket count).
@(define-syntax (test-query stx)
   (syntax-case stx ()
     [(test-query query str)
      #`(begin
          (interaction
           #:eval my-eval
           (define (q)
             #,(move #'here #'query -10))
           (display (to-sql (q))))
          (check-sql
           my-eval (q)
           str))]))
@(test-query
  (RS from x "X"
      (select (count x)))
  "select count(*) from X x")
@(test-query
  (RS from x "X"
      (select (count x".foo")))
  "select count(x.foo) from X x")
@(test-query
  (RS from x "X"
      (select (count x".foo" #:distinct? #t)))
  "select count(distinct x.foo) from X x")
@(test-query
  (RS from x "X"
      (join y "Y")
      (select (count y)))
  "select count(*) from X x inner join Y y on 1=1")
@(test-query
  (RS from x "X"
      (join y "Y")
      (select (count y".foo")))
  "select count(y.foo) from X x inner join Y y on 1=1")
@(test-query
  (RS from x "X"
      (join y "Y")
      (select (count y".foo" #:distinct? #t)))
  "select count(distinct y.foo) from X x inner join Y y on 1=1")
@(test-query
  (RS from x "X"
      (join y "Y"
            (group-by y".foo"))
      (select  (count y)))
  #<<HEREDOC
select y.__INJECT1
from X x
inner join (
  select count(*) as __INJECT1
  from Y y
  group by y.foo) y
on 1=1
HEREDOC
  )
@(test-query
  (RS from x "X"
      (join y "Y"
            (group-by y".foo"))
      (select (count y".bar")))
  #<<HEREDOC
select y.__INJECT1
from X x
inner join (
  select count(y.bar) as __INJECT1
  from Y y
  group by y.foo) y
on 1=1
HEREDOC
  )
@(test-query
  (RS from x "X"
      (join y "Y"
            (group-by y".foo"))
      (select (count y".bar" #:distinct? #t)))
  #<<HEREDOC
select y.__INJECT1
from X x
inner join (
  select count(distinct y.bar) as __INJECT1
  from Y y
  group by y.foo) y
on 1=1
HEREDOC
  )


OLD_SCRIBBLE_CODE
 )
