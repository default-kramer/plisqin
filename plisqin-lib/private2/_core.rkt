#lang racket

; This file is just a small insulation layer that
; provides bindings from Morsel that we want to use directly.
(provide tuple? get-join-type gen:queryable get-queryable
         ; Adding contracts makes doc-coverage consider them different from Morsel's
         ; identifiers, so it will alert if they are undocumented.
         (contract-out [to-sql (-> any/c string?)] ; TODO tighten this contract
                       [query? (-> any/c any/c)]
                       [join?  (-> any/c any/c)])
         )

(require morsel-lib
         morsel-lib/sql
         ; TODO fix this:
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
