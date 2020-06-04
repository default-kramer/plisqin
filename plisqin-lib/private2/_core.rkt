#lang racket

; This file is just a small insulation layer that
; provides bindings from Morsel that we want to use directly.
(provide tuple? get-join-type gen:queryable get-queryable
         query? join? to-sql safe-write
         )

(require morsel-lib
         morsel-lib/sql
         ; TODO fix these:
         (only-in morsel-lib/private/essence/from safe-write)
         (only-in morsel-lib/private/sql/clauses get-join-type))

; The following submodules expose bindings that should only be used in one small place,
; and then the rest of Plisqin should depend on that code instead.

(module+ from-helper
  (provide from join attach flatten-lists?)
  (require (submod morsel-lib/private/essence/from TODO-PRIVATE)))

(module+ special-clause-helper
  (require (submod morsel-lib/private/sql/clauses TODO-for-plisqin))
  (provide clause<%> content-set :limit :offset :distinct :join-type))

(module+ fragment-helper
  (provide sql-token<%> define-token-aspect-stuff))
