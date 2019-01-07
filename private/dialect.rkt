#lang racket

(provide postgres postgres? mssql mssql? sqlite sqlite?
         dialect? current-dialect)

(define dialect?
  (or/c 'postgres 'mssql 'sqlite #f))

(define-syntax-rule (def-dialect name name?)
  (begin
    (define (name) 'name)
    (define (name? x)
      (equal? 'name x))))
(def-dialect postgres postgres?)
(def-dialect mssql mssql?)
(def-dialect sqlite sqlite?)

(define current-dialect (make-parameter #f))