#lang racket

(provide define-schema get-schema schema?)

(module helper racket
  (provide (all-defined-out)
           current-connection)
  (require db "dialect.rkt"
           (only-in "show-table.rkt" current-connection)
           (prefix-in racket: racket))

  ; For now a schema is just a list of tables
  (struct schema (tables) #:transparent)
  (struct table (name columns) #:transparent)
  (struct column (name type pk? nullable?) #:transparent)

  ; A query appropriate for use with rows->schema
  (define mssql-query #<<HEREDOC
select c.TABLE_NAME, c.COLUMN_NAME, c.DATA_TYPE
	, case when exists (select *
		from INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc
		inner join INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
			on tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME
		where tc.CONSTRAINT_TYPE = 'PRIMARY KEY'
		and kcu.COLUMN_NAME = c.COLUMN_NAME
		and kcu.TABLE_NAME = c.TABLE_NAME
		and kcu.TABLE_SCHEMA = c.TABLE_SCHEMA) then 1 else 0 end as IsPrimaryKey
	, case when c.IS_NULLABLE='NO' then 0 else 1 end as IsNullable
from INFORMATION_SCHEMA.COLUMNS c
inner join INFORMATION_SCHEMA.TABLES t
	on t.TABLE_NAME = c.TABLE_NAME
	and t.TABLE_SCHEMA = c.TABLE_SCHEMA
where t.TABLE_TYPE = 'BASE TABLE'
order by t.TABLE_NAME, c.ORDINAL_POSITION
HEREDOC
    )

  ; (listof vector?) -> schema?
  ; Produces a schema given a result set (from query-rows).
  ; Assumes the columns are in the following order:
  ;   TableName, ColumnName, DataType, IsPrimaryKey (0 or 1), IsNullable (0 or 1)
  (define (rows->schema rows)
    (define groups (racket:group-by (λ(vec) (vector-ref vec 0))
                                    rows))
    (define tables
      (for/list ([group groups])
        (let ([table-name
               (vector-ref (car group) 0)]
              [columns
               (for/list ([vec group])
                 (column (vector-ref vec 1)
                         (vector-ref vec 2)
                         (if (equal? (vector-ref vec 3) 1)
                             'primary-key
                             #f)
                         (if (equal? (vector-ref vec 4) 1)
                             'nullable
                             #f)))])
          (table table-name columns))))
    (schema tables))

  ; connection? -> (listof table?)
  ; Inspects the database to return the table and column metadata.
  (define (get-tables conn)
    (define dialect (or (current-dialect)
                        (infer-dialect conn)))
    (cond
      [(postgres? dialect)
       (error "TODO PG")]
      [(mssql? dialect)
       (rows->schema (query-rows conn mssql-query))]
      ; For SQLite we have to iterate over each table; there is no way to get column info
      ; for multiple tables with one query.
      [(sqlite? dialect)
       (let* ([sql "select name from sqlite_master where type='table' order by name"]
              [table-names (query-list conn sql)])
         (for/list ([table-name table-names])
           ; It doesn't look like we can control which columns are returned by "pragma table_info".
           ; Hopefully these assumptions about column positions hold in all SQLite versions:
           ; 0 seems to be the 0-based index of the column
           ; 1 name
           ; 2 type
           ; 3 not-null?
           ; 4 default value
           ; 5 primary key index, where 0 means "not part of the PK"
           (let* ([sql (format "pragma table_info(~a);" table-name)]
                  [col-infos (query-rows conn sql)]
                  [columns
                   (for/list ([col-info col-infos])
                     (let* ([name (vector-ref col-info 1)]
                            [type (vector-ref col-info 2)]
                            [pk-val (vector-ref col-info 5)]
                            [pk? (if (equal? pk-val 0) #f 'primary-key)]
                            [not-null-val (vector-ref col-info 3)]
                            [nullable? (if (equal? not-null-val 0) 'nullable #f)])
                       (column name type pk? nullable?)))])
             (table table-name columns))))]
      [else (error "Unrecognized dialect:" dialect)]))

  ; connection? -> schema?
  (define (get-schema [conn (current-connection)])
    (schema (get-tables conn))))

(require 'helper (for-syntax 'helper db))
(require (only-in "schema.rkt"
                  def-table
                  def-fields-of))

(define-syntax-rule (2phase form)
  (begin form
         (begin-for-syntax form)))

(2phase
 ;;; schema->syntax
 ; schema? syntax? -> syntax?
 (define (schema->syntax schema [ctx #f])
   ;;; make
   ; schema? -> syntax?
   ; Returns a syntax object like
   ; ([table-id column-id ...] ...)
   (define (make x)
     (cond
       [(symbol? x)
        (datum->syntax ctx x ctx ctx)]
       [(string? x)
        (make (string->symbol x))]
       [(column? x)
        (make (column-name x))]
       [(table? x)
        #`[#,(make (table-name x))
           #,@(make (table-columns x))]]
       [(schema? x)
        (make (schema-tables x))]
       [(list? x)
        #`(#,@(map make x))]
       [else (error)]))
   (with-syntax ([([table-id column-id ...] ...)
                  (make schema)])
     #`(begin
         (def-table table-id)
         ...
         (def-fields-of table-id column-id ...)
         ...))))

; TODO tighten up with syntax/parse and contracts
; TODO assuming argument is (thunkof connection?), we should also
; support connection? (and then not disconnect).
(define-syntax (define-schema stx)
  (syntax-case stx ()
    [(_ create-conn)
     (let* ([conn (eval-syntax #'(create-conn))]
            [schema (get-schema conn)])
       (disconnect conn)
       (schema->syntax schema stx))]))

(begin
  (require (for-syntax "../examples/cities/city-connection.rkt"))
  (define-schema connect-cities)
  (City? (City)))

(module+ test
  (require rackunit)
  (current-connection 'cities-example)
  (let* ([schema (get-schema)]
         [stx (schema->syntax schema)])
    (check-equal? (syntax->datum stx)
                  '(begin
                     (def-table City)
                     (def-table Country)
                     (def-table CountryOrganization)
                     (def-table Organization)
                     (def-fields-of City
                       CityId CountryId CityName CityPopulation)
                     (def-fields-of Country
                       CountryId CountryName CountryPopulation NominalGDP)
                     (def-fields-of CountryOrganization
                       CountryId OrgId)
                     (def-fields-of Organization
                       OrgId OrgShortName))))

  ; convenience for building a list of tables
  (define-syntax-rule (make-tables [table-name [column-arg ...]
                                               ...]
                                   ...)
    (list (table table-name
                 (list (column column-arg ...)
                       ...))
          ...))

  (define fake-result-set
    '(#("T1" "c1" "varchar" 1 0)
      #("T1" "c2" "date" 0 0)
      #("T2" "c1" "int" 1 0)
      #("T2" "c2" "text" 0 1)))
  (check-equal? (schema-tables (rows->schema fake-result-set))
                (make-tables
                 ["T1" ["c1" "varchar" 'primary-key #f]
                       ["c2" "date" #f #f]]
                 ["T2" ["c1" "int" 'primary-key #f]
                       ["c2" "text" #f 'nullable]])))
