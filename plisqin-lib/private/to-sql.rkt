#lang racket
(require "core.rkt" "util.rkt" "ordered-joins.rkt" "deduplicate.rkt"
         "auto-inject.rkt" "auto-inject-aggregates.rkt" "make-unique-aliases.rkt"
         "resolve-injections.rkt" "dialect.rkt" "param-binder.rkt" "infer-join-type.rkt")
(module+ test
  (require rackunit)
  (require "macros.rkt")
  (define (check-sql q str)
    (check-equal?
     (string-normalize-spaces (to-sql q))
     (string-normalize-spaces str))))

(provide to-sql bind-sql rewrite)

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

; (or/c #f binder?)
(define current-binder (make-parameter #f))

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

(def/c (reduce x dialect)
  (-> sql-token? dialect? reduction?)
  ; helpers to recurse
  (def/c (recurse1 x)
    (-> (or/c sql-token? reduction?) reduction?)
    (cond
      [(list? x) x] ; Do we know this is fully reduced?
      [else (reduce x dialect)]))
  (def/c (recurseM things)
    (-> (listof (or/c sql-token? reduction?)) reduction?)
    (map recurse1 things))
  (def/c (go . args)
    (->* () #:rest (listof (or/c sql-token? reduction?)) reduction?)
    (recurseM args))
  ; helper to format the time unit
  (def/c (interval-unit iv)
    (-> interval? string?)
    (~a (s:time-unit-symbol (s:interval-unit iv))))
  ; main body
  (cond
    [(raw-sql? x) (raw-sql-content x)]
    [(value? x)
     (let ([content (value-content x)])
       (if (string? content)
           (go "'" (string-replace content "'" "''") "'")
           (~a content)))]
    [(param? x)
     (if (current-binder)
         (begin
           (current-binder (encounter (current-binder) x))
           ; Render a numeric parameter
           (let ([num (param-number (current-binder) x)])
             (cond
               [(postgres? dialect) (format "$~a" num)]
               [(sqlite? dialect) (format "?~a" num)]
               [(mssql? dialect) (format "@p~a" num)]
               [else (error "cannot render numbered parameter for dialect: " dialect)])))
         ; Else render a named parameter
         (cond
           [(sqlite? dialect)
            (format ":~a" (param-name x))]
           [(mssql? dialect)
            (format "@~a" (param-name x))]
           [else (error "cannot render named parameter for dialect: " dialect)]))]
    [(selected-bool? x)
     (let ([content (selected-bool-content x)])
       (cond
         [(mssql? dialect)
          (go "cast(case when "content" then 1 when not "content" then 0 end as bit)")]
         ; PG and SQLite handle this automatically
         [else (go content)]))]
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
    [(or (attached-join? x)
         (join? x))
     (recurse1 (get-src x))]
    [(source? x) (s:source-alias x)]
    [(subquery? x) (go "("(change-kind 'Sql x)")")]
    [(select? x)
     (let* ([as (select-as x)]
            [as (cond
                  [(not as) #f]
                  [(symbol? as) (format "~a" as)]
                  [(raw-sql? as) (raw-sql-content as)]
                  [(error "unhandled type of select-as" as)])])
       (go (recurseM (s:fragment-tokens x))
           (or (and as
                    (list 'SP "as" 'SP as))
               '())))]
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
              (let ([qty (s:interval-qty interval)])
                (go expr 'SP "+" 'SP
                    (if (number? qty)
                        (format "interval '~a ~a'"
                                qty
                                (interval-unit interval))
                        (go "("
                            qty " * "
                            (format "interval '1 ~a'"
                                    (interval-unit interval))
                            ")"))))
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
             (if (number? qty)
                 (go ", "
                     (format "'~a~a ~a'"
                             (if (positive? qty) "+" "")
                             qty
                             (interval-unit interval))
                     (recurse child))
                 (go ", "
                     qty
                     " || ' " (interval-unit interval) "'"
                     (recurse child))))))
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
       (go (help (s:dateadd-interval x) (s:dateadd-date x))))]
    [(dateadd? x)
     (error "cannot render date math for dialect:" dialect)]
    [(s:negation? x)
     (go "(-" (s:negation-val x) ")")]
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
         [join-type (infer-join-type j)]
         [type-string (match join-type
                        ['inner-join "inner join"]
                        ['left-join "left join"]
                        ['cross-apply "cross apply"]
                        ['outer-apply "outer apply"])]
         [is-apply (match join-type
                     ['cross-apply #t]
                     ['outer-apply #t]
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
                     (render-token (RS sql (raw-sql select-intro" ") src".*")))
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

(define/contract (bind-sql token)
  (-> sql-token? (cons/c string? binder?))
  (parameterize ([current-binder (new-binder)])
    (define sql (to-sql token))
    (cons sql (current-binder))))

(module+ test
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