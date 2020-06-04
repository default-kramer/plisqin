#lang racket

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

(require "./private2/token.rkt")
(provide >>)

(require "./private2/_null.rkt")
(provide nullability nullability? yes no maybe
         fallback fallback? /void /minval /maxval /any)

(require plisqin-lib/private2/define-schema)
(provide define-schema this)

(require "./private2/_statement.rkt")
(provide define-statement compile-statements)

(require (only-in "./private2/sql/frag-types.rkt" unsafe-content?))
(provide unsafe-content?)

(require "./private2/_core.rkt")
; Adding contracts makes doc-coverage consider them different from Morsel's
; identifiers, so it will alert if they are undocumented.
(provide (contract-out [to-sql (-> unsafe-content? string?)]
                       [query? (-> any/c any/c)]
                       [join?  (-> any/c any/c)]))

(require "./private2/sql/special-clauses.rkt")
(provide limit offset distinct join-type)

(require "./private2/from.rkt")
(provide from join instance? instanceof)

(require "./private2/sql/interval.rkt")
(provide interval?)

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
