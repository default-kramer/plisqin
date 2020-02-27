#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(require "./private2/sql/fragment.rkt")
(provide >>)

(require "./private2/_null.rkt")
(provide yes no maybe /void /minval /maxval /any)

(require plisqin-lib/private2/define-schema)
(provide define-schema this)

;(require plisqin-lib/private/api)
;(provide (all-from-out plisqin-lib/private/api))
;(require plisqin-lib/private/schema)
;(provide (all-from-out plisqin-lib/private/schema))
;(require plisqin-lib/private/aggregates)
;(provide (all-from-out plisqin-lib/private/aggregates))

;(require plisqin-lib/private/schema/schema)
;(provide define-schema this)

(require plisqin-lib/private2/show-table)
(provide show-table current-connection)

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
