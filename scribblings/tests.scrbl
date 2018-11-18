#lang scribble/manual
@(require (for-label plisqin
                     "racket.rkt"
                     plisqin/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          (except-in pict table)
          "racket.rkt")

@title{Tests}
This section is just a dumping ground for tests that might be educational.

@(define (test)
   (let ([e (filled-ellipse 4 4)])
     (centered (hc-append 20 e e e))))

@(test)
Aliases are made to be unique if needed.
Both of these want the alias "x" but the subquery gets renamed to "x1":

@(reset-eval!)
@(interaction
  #:eval my-eval
  (define (my-subquery parent)
    (from x "Sub"
          (where x".Something = "parent".Something")))
  (define my-query
    (from x "Parent"
          (where "not "(exists (my-subquery x)))))
  (display (to-sql my-query)))

@(check-sql
  my-eval my-query
  #<<HEREDOC
select x.*
from Parent x
where not exists (
    select x1.*
    from Sub x1
    where x1.Something = x.Something)
HEREDOC
  )

@(test)
Joins are included even if they don't appear in a fragment:

@(reset-eval!)
@(interaction
  #:eval my-eval
  (define my-query
    (from x "X"
          (join y "Y"
                (join-on "1=1"))))
  (display (to-sql my-query)))

@(check-sql
  my-eval my-query
  #<<HEREDOC
select x.*
from X x
inner join Y y on 1=1
HEREDOC
  )

@(test)
The query-building macros accept a single clause or a list for greater composability:

@(interaction
  #:eval my-eval
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
  (display (to-sql my-query)))
@(check-sql
  my-eval my-query
  #<<HEREDOC
select
  x.First
  , x.Second
  , x.Third
from X x
HEREDOC
  )

@(test)
Aggregate functions don't need to be injected if they refer only to the main query and "simple" joins:
@(interaction
  #:eval my-eval
  (define my-query
    (from t "Title"
          (join r "Rating"
                (join-on r".TitleID = "t".TitleID"))
          (select (sum t".TitleID + "r".Score"))
          (group-by t".TitleID")))
  (display (to-sql my-query)))
@(check-sql
  my-eval my-query
  #<<HEREDOC
select sum(t.TitleID + r.Score)
from Title t
inner join Rating r on r.TitleID = t.TitleID
group by t.TitleID
HEREDOC
  )

@(test)
The SQL `having' clause:
@(interaction
  #:eval my-eval
  (define my-query
    (from r "Rating"
          (group-by r".TitleID")
          (having "count("r".TitleID) > 100")
          (select r".TitleID")
          (select (avg r".Score"))))
  (display (to-sql my-query)))
@(check-sql
  my-eval my-query
  #<<HEREDOC
select
  r.TitleID
  , avg(r.Score)
from Rating r
group by r.TitleID
having count(r.TitleID) > 100
HEREDOC
  )

@(test)
We can deduplicate injections!
@(interaction
  #:eval my-eval
  (define my-query
    (from t "Title"
          (join ratings "Rating"
                (group-by ratings".TitleID")
                (join-on (scalar ratings".TitleID")" = "t".TitleID"))
          (select (avg ratings".Score")" as AvgScore")
          (select (avg ratings".Score")" as AvgScore2")))
  (display (to-sql my-query)))
@(reset-uid)
@(check-sql
  my-eval my-query
  #<<HEREDOC
select
  ratings.__INJECT2 as AvgScore
  , ratings.__INJECT2 as AvgScore2
from Title t
inner join (
    select
      ratings.TitleID as __INJECT1
      , avg(ratings.Score) as __INJECT2
    from Rating ratings
    group by ratings.TitleID) ratings
   on ratings.__INJECT1 = t.TitleID
HEREDOC
  )

@(test)
The old resolve-joins was designed on the assumption that we would not find the source of
a join in the fragments. In other words, that all joins would be inlined.
This is no longer true. Let's try to create a failing test here:
@(interaction
  #:eval my-eval
  (define (TitleType-of/s title)
    (join tt "TitleType"
          (join-on tt".TitleTypeID = "title".TitleTypeID")))

  (define my-query
    (from t "Title"
          (join tt1 (TitleType-of/s t))
          (join tt2 (TitleType-of/s t))
          (select tt1".a1")
          (select tt2".a2")
          (select (TitleType-of/s t)".a3")
          (select (TitleType-of/s t)".a4")))
  (display (to-sql my-query)))
@(check-sql
  my-eval my-query
  #<<HEREDOC
select tt.a1 , tt.a2 , tt.a3 , tt.a4
from Title t
inner join TitleType tt
   on tt.TitleTypeID = t.TitleTypeID
HEREDOC
  )

@(test)
Regression test. A joined subquery should be able to explicitly join other other joins.
@(interaction
  #:eval my-eval
  (define my-query
    (from x "X"
          (join y "Y"
                (join-on "'y'='y'")
                (join z "Z"
                      (join-on "'z'='z'"))
                (select y".Foo")
                (select z".Bar"))))
  (display (to-sql my-query)))
@(check-sql
  my-eval my-query
  #<<HEREDOC
select x.*
from X x
inner join (
    select
      y.Foo
      , z.Bar
    from Y y
    inner join Z z
       on 'z'='z') y
   on 'y'='y'
HEREDOC
  )

@(test)
Auto-inject scalars in join-on clauses:
@(interaction
  #:eval my-eval
  (define my-query
    (from x "X"
          (join y "Y"
                (group-by y".XID")
                (join-on (scalar y".XID")" = "(scalar x".XID")))
          (select x".*")
          (select (count y)" as NumYs")))
  (display (to-sql my-query)))
@(check-sql
  my-eval my-query
  #<<HEREDOC
select
  x.*
  , y.__INJECT2 as NumYs
from X x
inner join (
    select
      y.XID as __INJECT1
      , count(*) as __INJECT2
    from Y y
    group by y.XID) y
on y.__INJECT1 = x.XID
HEREDOC
  )

@(test)
Auto-inject scalars in join-on clauses (TODO no tests here)
@(interaction
  #:eval my-eval
  (require (only-in racket remove-duplicates))
  (define (jZ y [group? #f])
    (join z "Z"
          (if group?
              (group-by (scalar z".ZID"))
              (list))
          (join-on (scalar z".ZID")" = "(scalar y".ZID"))))
  (define (jY x [group? #f])
    (join y "Y"
          (if group?
              (group-by (scalar y".YID"))
              (list))
          (join-on (scalar y".YID")" = "(scalar x".YID"))))
  ; Use a macro to test various combinations of inline and non-inline joins.
  (define-syntax-rule (q1 linkY linkZ)
    (from x "X"
          ;(join force-this-to-render-first (jY x))
          (linkY y (jY x))
          (linkZ z (jZ y #t))
          (select (count z)" as Blah")))
  ; The generated SQL should be the same for all variants
  (define sql-strings
    (remove-duplicates
     (list
      (to-sql (q1 join join))
      "\n--- a ---\n"
      (to-sql (q1 join define))
      "\n--- b ---\n"
      (to-sql (q1 define join))
      "\n--- c ---\n"
      (to-sql (q1 define define)))))
  (length sql-strings)
  (displayln sql-strings))

@(test)
Auto-inject with duplicate inline grouped joins (TODO no tests here)
@(interaction
  #:eval my-eval
  (define my-query
    (from x "X"
          (define (z) (jZ (jY x) #t))
          (select (count (z))" as Blah")
          (select (sum (z)".Stuff")" as Total")))
  (displayln (to-sql my-query)))

@(test)
Regression test. This was broken because the auto-injected scalars used different sources (from the
questionable global scope) which broke equality for the "auto-inject aggregates" algorithm.
The solution is to call deduplicate between both algorithms.
The better solution would be some way to avoid the global scope when creating sources.
@(interaction
  #:eval my-eval
  (define (my-query)
    (from cust Customer
          (define (is-movie rental)
            (bool (ItemTypeId rental)" = 1"))
          (define (is-recent rental)
            (bool (CheckoutTime rental)" > '2018-01-01'"))
          (define (count-when predicate)
            (sum "case when "predicate" then 1 else 0 end"))
          (join rentals (Rentals-of/g cust))
          (select cust".*")
          (select (count rentals)
                  " as TotalRentalCount")
          (select (count-when (is-movie rentals))
                  " as MovieRentalCount")
          (select (count-when (is-recent rentals))
                  " as RecentRentalCount")
          (select (count-when (bool (is-movie rentals)
                                    " and "
                                    (is-recent rentals)))
                  " as RecentMovieRentalCount")))
  (display (to-sql (my-query))))
@(check-sql
  my-eval (my-query)
  #<<HEREDOC
select
  cust.*
  , _rental.__INJECT2 as TotalRentalCount
  , _rental.__INJECT3 as MovieRentalCount
  , _rental.__INJECT4 as RecentRentalCount
  , _rental.__INJECT5 as RecentMovieRentalCount
from Customer cust
inner join (
    select
      _checkout.CustomerId as __INJECT1
      , count(*) as __INJECT2
      , sum(case when _item.ItemTypeId = 1 then 1 else 0 end) as __INJECT3
      , sum(case when _checkout.CheckoutTime > '2018-01-01' then 1 else 0 end) as __INJECT4
      , sum(case when _item.ItemTypeId = 1 and _checkout.CheckoutTime > '2018-01-01' then 1 else 0 end) as __INJECT5
    from Rental _rental
    inner join Checkout _checkout
    on _checkout.CheckoutId = _rental.CheckoutId
    inner join Copy _copy
    on _copy.CopyId = _rental.CopyId
    inner join Item _item
    on _item.ItemId = _copy.ItemId
    group by _checkout.CustomerId) _rental
on _rental.__INJECT1 = cust.CustomerId
HEREDOC
  )

@(module HELP racket
   ; Move a syntax object to a new location so that Scribble sets it properly.
   ; Only changes line and column, that is all the seems to be needed.
   ; TODO this is probably reinventing some wheel... if not, it seems like a missing
   ; feature of Scribble.
   (provide move)
   (define (getloc stx)
     (list
      (syntax-source stx)
      (syntax-line stx)
      (syntax-column stx)
      (syntax-position stx)
      (syntax-span stx)))
   (define (move dest src [col-offset 0])
     (define-values (line-offset col)
       (match (list (getloc dest) (getloc src))
         [(list (list a1 b1 c1 d1 e1)
                (list a2 b2 c2 d2 e2))
          (values
           (- b2 b1)
           (- c2 c1 col-offset))]))
     (define (go stx)
       (define children (syntax->list stx))
       (define me (if children
                      (map go children)
                      (syntax->datum stx)))
       (datum->syntax stx me (list
                              (syntax-source dest)
                              (- (syntax-line stx) line-offset)
                              (- (syntax-column stx) col)
                              (- (syntax-position stx) 0)
                              (- (syntax-span stx) 0))))
     (go src)))
@(require (for-syntax 'HELP))

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
  (from x "X"
        (select (count x)))
  "select count(*) from X x")
@(test-query
  (from x "X"
        (select (count x".foo")))
  "select count(x.foo) from X x")
@(test-query
  (from x "X"
        (select (count x".foo" #:distinct? #t)))
  "select count(distinct x.foo) from X x")
@(test-query
  (from x "X"
        (join y "Y")
        (select (count y)))
  "select count(*) from X x inner join Y y on 1=1")
@(test-query
  (from x "X"
        (join y "Y")
        (select (count y".foo")))
  "select count(y.foo) from X x inner join Y y on 1=1")
@(test-query
  (from x "X"
        (join y "Y")
        (select (count y".foo" #:distinct? #t)))
  "select count(distinct y.foo) from X x inner join Y y on 1=1")
@(test-query
  (from x "X"
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
  (from x "X"
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
  (from x "X"
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

@(test)
Tests of @(racket exists).
@(test-query
  (from x "X"
        (where (exists "select * from blah")))
  #<<HEREDOC
select x.*
from X x
where exists (select * from blah)
HEREDOC
  )
@(test-query
  (from x "X"
        (where (exists (from y "Y"
                             (where y".Foo = "x".Bar")))))
  #<<HEREDOC
select x.*
from X x
where exists ( select y.* from Y y where y.Foo = x.Bar)
HEREDOC
  )
@(test-query
  (from x "X"
        (where (exists (join y "Y"
                             (join-on y".Foo = "x".Bar")))))
  #<<HEREDOC
select x.*
from X x
where exists ( select y.* from Y y where y.Foo = x.Bar)
HEREDOC
  )