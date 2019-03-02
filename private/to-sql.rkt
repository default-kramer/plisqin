#lang racket
(require "core.rkt" "util.rkt" "ordered-joins.rkt" "deduplicate.rkt"
         "auto-inject.rkt" "auto-inject-aggregates.rkt" "make-unique-aliases.rkt"
         "resolve-injections.rkt" "dialect.rkt")
(module+ test
  (require rackunit)
  (require "macros.rkt")
  (define (check-sql q str)
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

; A Reduction is an unflattened list of strings (or a single string).
; The symbol 'SP represents a space that can be collapsed.
(define reduction? (or/c string? 'SP (recursive-contract (listof reduction?))))

(define/contract (reduce x dialect)
  (-> sql-token? dialect? reduction?)
  ; helpers to recurse
  (define/contract (recurse1 x)
    (-> (or/c sql-token? reduction?) reduction?)
    (cond
      [(list? x) x] ; Do we know this is fully reduced?
      [else (reduce x dialect)]))
  (define/contract (recurseM things)
    (-> (listof (or/c sql-token? reduction?)) reduction?)
    (map recurse1 things))
  (define/contract (go . args)
    (->* () #:rest (listof (or/c sql-token? reduction?)) reduction?)
    (recurseM args))
  ; helper to format the time unit
  (define/contract (interval-unit iv)
    (-> interval? string?)
    (~a (s:time-unit-symbol (s:interval-unit iv))))
  ; main body
  (cond
    [(equal? 'SP x) 'SP]
    [(equal? 'db-now x)
     (cond
       [(postgres? dialect) "current_timestamp"]
       [(mssql? dialect) "getdate()"]
       [(sqlite? dialect) "datetime('now')"]
       [else (error "cannot render db-now for dialect: " dialect)])]
    [(equal? 'concat x)
     (cond
       [(mssql? dialect) " + "]
       [else " || "])]
    [(number? x) (~a x)]
    [(string? x) x]
    [(silence? x) ""]
    [(or (binding? x)
         (join? x))
     (recurse1 (get-src x))]
    [(source? x) (s:source-alias x)]
    [(subquery? x) (go "("(change-kind 'Sql x)")")]
    [(fragment? x)
     (recurseM (s:fragment-tokens x))]
    [(query? x)
     (if (root-query?)
         (parameterize ([root-query? #f])
           (render-query (rewrite x)))
         (indent (string-append "\n" (render-query x))))]
    [(cases? x)
     (go "case" 'SP
         (or (s:cases-of x) 'SP)
         (map (λ(pair)
                (go 'SP "when" 'SP (car pair)
                    'SP "then" 'SP (cdr pair)))
              (s:cases-contents x))
         (if (s:cases-else x)
             (go 'SP "else" 'SP (s:cases-else x))
             'SP)
         'SP "end")]
    ; dateadd, postgres
    [(and (dateadd? x)
          (postgres? dialect))
     (begin
       (define (recurse expr interval)
         (if (not interval)
             expr
             (recurse
              (go expr 'SP "+" 'SP
                  (format "interval '~a ~a'"
                          (s:interval-qty interval)
                          (interval-unit interval)))
              (s:interval-added-to interval))))
       (go 'SP "(" (recurse (s:dateadd-date x) (s:dateadd-interval x)) ")"))]
    ; dateadd, sqlite
    [(and (dateadd? x)
          (sqlite? dialect))
     (define (recurse interval)
       (if (not interval)
           ""
           (let ([qty (s:interval-qty interval)]
                 [child (s:interval-added-to interval)])
             (format ", '~a~a ~a'~a"
                     (if (negative? qty) "" "+")
                     qty
                     (interval-unit interval)
                     (recurse child)))))
     (go 'SP "datetime(" (s:dateadd-date x) (recurse (s:dateadd-interval x)) ")")]
    ; dateadd, mssql
    [(and (dateadd? x)
          (mssql? dialect))
     (begin
       (define/contract (help iv operand)
         (-> (or/c interval? #f) any/c reduction?)
         (if iv
             (help (s:interval-added-to iv)
                   (go "dateadd("
                       (~a (interval-unit iv))
                       "," 'SP (s:interval-qty iv)
                       "," 'SP operand ")"))
             operand))
       (go 'SP (help (s:dateadd-interval x) (s:dateadd-date x))))]
    [(dateadd? x)
     (error "cannot render date math for dialect:" dialect)]
    ; We do not expect joins or injections here
    [else (error "got unexpected token: " x)]))

(define/contract (render-token tok)
  (-> sql-token? string?)
  (define (skip-opening-SPs lst)
    (match lst
      [(list 'SP rest ...)
       (skip-opening-SPs rest)]
      [else lst]))
  (define (collapse-SPs lst)
    (match lst
      [(list 'SP 'SP rest ...)
       (collapse-SPs (cons 'SP rest))]
      [(list 'SP rest ...)
       (cons " " (collapse-SPs rest))]
      [(list a rest ...)
       (cons a (collapse-SPs rest))]
      [(list) (list)]))
  (define flattened (flatten (reduce tok (current-dialect))))
  (string-join (collapse-SPs (skip-opening-SPs flattened)) ""))

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
                        (render-token (s:source-table src)))]
         [is-ms? (mssql? (current-dialect))]
         [limit ; (or/c number? #f)
          (query-limit q)]
         [offset ; (or/c number? #f)
          (query-offset q)]
         [distinct? ; boolean
          (query-distinct? q)]
         ; for SQL Server, if there is no offset use "top" instead
         [top ; (or/c number? #f)
          (if (and is-ms? limit (not offset))
              limit
              #f)]
         [select-intro ; will be "select [distinct] [top N]"
          (string-append "select"
                         (if distinct?
                             " distinct"
                             "")
                         (if top
                             (format " top ~a" top)
                             ""))])
    (string-append
     (render-clauses (query-fragments q 'Select)
                     (string-append select-intro "\n  ")
                     "\n  , "
                     (render-token (sql select-intro" "src".*")))
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
                     "")
     ; non-MS uses "limit" and "offset"
     (if (and (not is-ms?) limit)
         (format "\nlimit ~a" limit)
         "")
     (if (and (not is-ms?) offset)
         (format "\noffset ~a" offset)
         "")
     ; MS uses "offset" and "fetch next"
     (if (and is-ms? offset)
         (format "\noffset ~a rows" offset)
         "")
     (if (and is-ms? limit (not top))
         (format "\nfetch next ~a rows only" limit)
         ""))))

(define/contract (to-sql token)
  (-> sql-token? string?)
  (render-token token))

(module+ test
  (require (prefix-in op: "operators.rkt"))
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
   )

  (define-syntax-rule (with-MS forms ...)
    (parameterize ([current-dialect (mssql)])
      forms ...))
  (define-syntax-rule (with-PG forms ...)
    (parameterize ([current-dialect (postgres)])
      forms ...))

  (define frag (op:+ (sql "getdate()")
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
  (set! frag (op:- (sql "getdate()")
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