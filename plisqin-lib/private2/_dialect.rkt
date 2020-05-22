#lang racket

(provide (contract-out
          [dialect? (-> any/c any/c)]
          [current-dialect (parameter/c (or/c #f dialect?))]
          [mssql (-> dialect?)]
          [mssql? (-> any/c any/c)]
          [postgres (-> dialect?)]
          [postgres? (-> any/c any/c)]
          [sqlite (-> dialect?)]
          [sqlite? (-> any/c any/c)]
          ))

(require morsel-lib/sql/dialect)
