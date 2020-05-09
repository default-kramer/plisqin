#lang racket

(require "helpers.rkt"
         (only-in plisqin from join distinct limit offset))

; Test combinations of limit, offset, and distinct.
; SQL Server with no offset uses "top" instead of "limit"
(check-sql
 (from x 'X
       (limit 5))
 #:all "select x.* from X x limit 5"
 #:ms "select top 5 x.* from X x")
; If offset and limit are both set, then SQL Server can't use "top"
(check-sql
 (from x 'X
       (limit 5)
       (offset 9))
 #:all "select x.* from X x limit 5 offset 9"
 #:ms "select x.* from X x offset 9 rows fetch next 5 rows only")
; Repeat the tests with distinct added
(check-sql
 (from x 'X
       (distinct #t)
       (limit 5))
 #:all "select distinct x.* from X x limit 5"
 #:ms "select distinct top 5 x.* from X x")
(check-sql
 (from x 'X
       (distinct #t)
       (limit 5)
       (offset 9))
 #:all "select distinct x.* from X x limit 5 offset 9"
 #:ms "select distinct x.* from X x offset 9 rows fetch next 5 rows only")
