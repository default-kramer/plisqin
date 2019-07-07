#lang reader "reader.rkt"
(require "bug-1-helper.rkt"
         rackunit)

; def/append! needs to do an append! here, even though TableBar was
; defined in another file
(def/append! (TableBar x)
  [(TableFoo? x)
   "a join"])

(check-true (TableBar? (TableBar)))
(check-true (TableBar? (from t TableBar)))
(check-equal? (TableBar (TableFoo))
              "a join")