#lang racket
(require "core.rkt" "util.rkt" "ordered-joins.rkt" "deduplicate.rkt")
(require "auto-inject.rkt" "auto-inject-aggregates.rkt" "make-unique-aliases.rkt" "resolve-injections.rkt")
(module+ test
  (require rackunit)
  (require "macros.rkt")
  (define-syntax-rule (check-sql q str)
    (check-equal?
     (string-normalize-spaces (to-sql q))
     (string-normalize-spaces str))))

(provide to-sql rewrite)

(define/contract (rewrite q)
  (-> query? query?)
  (let* ([q (deduplicate q)]
         [q (auto-inject q)]
         [q (deduplicate q)]
         [q (inject-aggregates q)]
         [q (deduplicate q)]
         [q (resolve-injections q)]
         [q (make-unique-aliases q)])
    q))

; (: indent (-> String String))
(define (indent str)
  (string-replace str "\n" "\n    "))

; When we see a root query, we want to rewrite it, but then as we dive deeper
; we don't want to "re-rewrite" any subqueries. Use a parameter to know whether we are
; on the root query or not.
(define root-query? (make-parameter #t))

(define/contract (render-token tok)
  (-> sql-token? string?)
  (cond
    [(silence? tok) ""]
    [(binding? tok) (render-token (get-src tok))]
    [(join? tok) (render-token (get-src tok))]
    [(string? tok) tok]
    [(source? tok) (s:source-alias tok)]
    [(subquery? tok) (string-append "(" (render-token (change-kind 'Sql tok)) ")")]
    [(fragment? tok) (apply string-append (map render-token (s:fragment-tokens tok)))]
    [(query? tok)
     (if (root-query?)
         (parameterize ([root-query? #f])
           (render-query (rewrite tok)))
         (indent (string-append "\n" (render-query tok))))]
    [(number? tok) (format "~a" tok)]
    ; We do not expect joins or injections here
    [#t (error "got unexpected token: " tok)]))

(define/contract (render-clauses frags intro joiner if-empty)
  (-> (listof fragment?) string? string? string? string?)
  (if (empty? frags)
      if-empty
      (let ([strs (map render-token frags)])
        (string-append intro (string-join strs joiner)))))

(define/contract (render-join j)
  (-> join? string?)
  (let* ([src (get-src j)]
         [query (s:join-query j)]
         [type-string (match (s:join-type j)
                        ['InnerJoin "inner join"]
                        ['LeftJoin "left join"]
                        ['CrossApply "cross apply"]
                        ['OuterApply "outer apply"])]
         [is-apply (match (s:join-type j)
                     ['CrossApply #t]
                     ['OuterApply #t]
                     [else #f])]
         [default-on-string (if is-apply
                                ""
                                "\non 1=1")]
         [join-body (if (is-simple-join? j)
                        (s:source-table src)
                        (string-append "(" (render-token query) ")"))])
    (string-append
     "\n"type-string" "join-body" "(s:source-alias src)
     (render-clauses (s:join-clauses j)
                     "\non "
                     "\nand "
                     default-on-string))))

(define/contract (render-query q)
  (-> query? string?)
  (let* ([src (s:query-source q)]
         [table-str (if (string? (s:source-table src))
                        (s:source-table src)
                        (render-token (s:source-table src)))])
    (string-append
     (render-clauses (query-fragments q 'Select)
                     "select\n  "
                     "\n  , "
                     (render-token (sql "select "src".*")))
     "\nfrom "table-str" "(s:source-alias src)
     (string-join (map render-join (ordered-joins q)) "")
     (render-clauses (query-fragments q 'Where)
                     "\nwhere "
                     "\nand "
                     "")
     (render-clauses (query-fragments q 'GroupBy)
                     "\ngroup by "
                     ", "
                     "")
     (render-clauses (query-fragments q 'Having)
                     "\nhaving "
                     "\n   and "
                     "")
     (render-clauses (query-fragments q 'OrderBy)
                     "\norder by "
                     ", "
                     ""))))

(define/contract (to-sql token)
  (-> sql-token? string?)
  (render-token token))

(module+ test
  (check-sql
   (from a "A")
   "select a.* from A a")
  
  (check-sql
   (from t "Title"
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
   (from t "Title"
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
   (from x "X"
         (join y "Y" 'LeftJoin
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
   (from x "X"
         (join y "Y" 'CrossApply
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
   (from x (subquery "select 1 as ONE")
         (select x".ONE"))
   #<<HEREDOC
select x.ONE
from (select 1 as ONE) x
HEREDOC
   )
  (check-sql
   (from x (subquery (from y "Y"
                           (select y".ONE")))
         (select x".ONE"))
   #<<HEREDOC
select x.ONE
from (
    select y.ONE from Y y) x
HEREDOC
   ))