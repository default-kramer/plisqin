#lang racket

; This module works with symbols; it is unaware of "real" fallbacks.
; For example, this code recognizes '/void but doesn't know that /void even exists.
; The SQL rendering logic will have to do a small translation from real
; fallbacks to their symbolic equivalents.
(provide probe truth-test)

(module help racket
  (provide probe ops op? fallbacks fallback-sym? truth-test)

  (define-syntax-rule (define-symlist id contract-id? [sym ...])
    (begin
      (define id '(sym ...))
      (define contract-id? (or/c 'sym ...))))

  (define-symlist ops op? [= <> < > <= >=])
  (define-symlist fallbacks fallback-sym? [/void /minval /maxval /any])


  ; We write our matchers assuming two things:
  ; 1) /void is handled at a higher level (it always implies #f)
  ; 2) At least one of the operands is a fallback
  ; This makes the matchers easier to write.
  (define-syntax-rule (matcher [x y z] ... [else-keyword else-body])
    (lambda (pattern)
      (match pattern
        [(list x y) z]
        ...
        [else-keyword else-body])))

  (define =matcher
    (matcher
     [x x #t]
     ['/any x #t]
     [x '/any #t]
     [else #f]))

  (define <>matcher
    (matcher
     ['/any x #t]
     [x '/any #t]
     [x x #f]
     [else #t]))

  (define <matcher
    (matcher
     ['/any '/any #t]
     [x x #f]
     [x '/minval #f]
     ['/minval x #t]
     [x '/maxval #t]
     ['/maxval x #f]
     ['/any x #t]
     [x '/any #t]
     [else #f]))

  (define >matcher
    (matcher
     ['/any '/any #t]
     [x x #f]
     [x '/minval #t]
     ['/minval x #f]
     [x '/maxval #f]
     ['/maxval x #t]
     ['/any x #t]
     [x '/any #t]
     [else #f]))

  (define (tester-help pattern)
    (define proc
      (case (car pattern)
        [(=) =matcher]
        [(<>) <>matcher]
        [(<) <matcher]
        [(>) >matcher]
        [(<=) (λ (x) (or (<matcher x)
                         (=matcher x)))]
        [(>=) (λ (x) (or (>matcher x)
                         (=matcher x)))]
        [else (error "Unrecognized operator:" (car pattern))]))
    (proc (cdr pattern)))

  (define (tester pattern)
    (match pattern
      [(list op '/void b) #f]
      [(list op a '/void) #f]
      [else (tester-help pattern)]))

  (define/contract (truth-test pattern)
    (-> (list/c op? any/c any/c) boolean?)
    (tester pattern))

  ; Two unique values that can stand in for any real value
  (define real-lhs (gensym))
  (define real-rhs (gensym))

  ; probe takes 3 arguments
  ;   op  : `op?`
  ;   lhs : (or/c fallback-sym? any/c)
  ;     A non-fallback-sym? value indicates "no fallback"
  ;   rhs : (or/c fallback-sym? any/c)
  ;     A non-fallback-sym? value indicates "no fallback"
  ; Returns a "probe result", a list of three booleans:
  ;   1 : Should the result be true when only lhs is dbnull?
  ;   2 : Should the result be true when only rhs is dbnull?
  ;   3 : Should the result be true when both are dbnull?
  (define (probe op lhs rhs)
    (let ([c1 (and (fallback-sym? lhs)
                   (tester (list op lhs real-rhs)))]
          [c2 (and (fallback-sym? rhs)
                   (tester (list op real-lhs rhs)))]
          [c3 (and (or (fallback-sym? lhs)
                       (fallback-sym? rhs))
                   (tester (list op lhs rhs)))])
      (list c1 c2 c3))))

(require 'help)

(define-syntax (def-truth-table stx)
  (syntax-case stx ()
    [(_ id stx-id form)
     #'(begin
         ; Capture as a list for testing
         (module+ test
           (define id
             (quote form)))
         ; Capture as a syntax object for Scribble
         (module+ for-scribble
           (provide stx-id)
           (define stx-id (syntax form))))]))

(def-truth-table truth-table truth-table-as-stx
  ([#f      (= /void /void)]
   [#f      (= /void /minval)]
   [#f      (= /void /maxval)]
   [#f      (= /void /any)]
   [#f      (= /void 42)]

   [#f      (= /minval /void)]
   [#true   (= /minval /minval)]
   [#f      (= /minval /maxval)]
   [#true   (= /minval /any)]
   [#f      (= /minval 42)]

   [#f      (= /maxval /void)]
   [#f      (= /maxval /minval)]
   [#true   (= /maxval /maxval)]
   [#true   (= /maxval /any)]
   [#f      (= /maxval 42)]

   [#f      (= /any /void)]
   [#true   (= /any /minval)]
   [#true   (= /any /maxval)]
   [#true   (= /any /any)]
   [#true   (= /any 42)]

   [#f      (<> /void /void)]
   [#f      (<> /void /minval)]
   [#f      (<> /void /maxval)]
   [#f      (<> /void /any)]
   [#f      (<> /void 42)]

   [#f      (<> /minval /void)]
   [#f      (<> /minval /minval)]
   [#true   (<> /minval /maxval)]
   [#true   (<> /minval /any)]
   [#true   (<> /minval 42)]

   [#f      (<> /maxval /void)]
   [#true   (<> /maxval /minval)]
   [#f      (<> /maxval /maxval)]
   [#true   (<> /maxval /any)]
   [#true   (<> /maxval 42)]

   [#f      (<> /any /void)]
   [#true   (<> /any /minval)]
   [#true   (<> /any /maxval)]
   [#true   (<> /any /any)]
   [#true   (<> /any 42)]

   [#f      (< /void /void)]
   [#f      (< /void /minval)]
   [#f      (< /void /maxval)]
   [#f      (< /void /any)]
   [#f      (< /void 42)]

   [#f      (< /minval /void)]
   [#f      (< /minval /minval)]
   [#true   (< /minval /maxval)]
   [#true   (< /minval /any)]
   [#true   (< /minval 42)]

   [#f      (< /maxval /void)]
   [#f      (< /maxval /minval)]
   [#f      (< /maxval /maxval)]
   [#f      (< /maxval /any)]
   [#f      (< /maxval 42)]

   [#f      (< /any /void)]
   [#f      (< /any /minval)]
   [#true   (< /any /maxval)]
   [#true   (< /any /any)]
   [#true   (< /any 42)]

   [#f      (> /void /void)]
   [#f      (> /void /minval)]
   [#f      (> /void /maxval)]
   [#f      (> /void /any)]
   [#f      (> /void 42)]

   [#f      (> /minval /void)]
   [#f      (> /minval /minval)]
   [#f      (> /minval /maxval)]
   [#f      (> /minval /any)]
   [#f      (> /minval 42)]

   [#f      (> /maxval /void)]
   [#true   (> /maxval /minval)]
   [#f      (> /maxval /maxval)]
   [#true   (> /maxval /any)]
   [#true   (> /maxval 42)]

   [#f      (> /any /void)]
   [#true   (> /any /minval)]
   [#f      (> /any /maxval)]
   [#true   (> /any /any)]
   [#true   (> /any 42)]

   [#f      (<= /void /void)]
   [#f      (<= /void /minval)]
   [#f      (<= /void /maxval)]
   [#f      (<= /void /any)]
   [#f      (<= /void 42)]

   [#f      (<= /minval /void)]
   [#true   (<= /minval /minval)]
   [#true   (<= /minval /maxval)]
   [#true   (<= /minval /any)]
   [#true   (<= /minval 42)]

   [#f      (<= /maxval /void)]
   [#f      (<= /maxval /minval)]
   [#true   (<= /maxval /maxval)]
   [#true   (<= /maxval /any)]
   [#f      (<= /maxval 42)]

   [#f      (<= /any /void)]
   [#true   (<= /any /minval)]
   [#true   (<= /any /maxval)]
   [#true   (<= /any /any)]
   [#true   (<= /any 42)]

   [#f      (>= /void /void)]
   [#f      (>= /void /minval)]
   [#f      (>= /void /maxval)]
   [#f      (>= /void /any)]
   [#f      (>= /void 42)]

   [#f      (>= /minval /void)]
   [#true   (>= /minval /minval)]
   [#f      (>= /minval /maxval)]
   [#true   (>= /minval /any)]
   [#f      (>= /minval 42)]

   [#f      (>= /maxval /void)]
   [#true   (>= /maxval /minval)]
   [#true   (>= /maxval /maxval)]
   [#true   (>= /maxval /any)]
   [#true   (>= /maxval 42)]

   [#f      (>= /any /void)]
   [#true   (>= /any /minval)]
   [#true   (>= /any /maxval)]
   [#true   (>= /any /any)]
   [#true   (>= /any 42)]))

(module+ test
  (require rackunit)

  (for ([item truth-table])
    (let* ([expected (car item)]
           [pattern (cadr item)]
           [actual (truth-test pattern)])
      (check-equal? actual expected (format "checking truth table: ~a" item))))

  (for* ([lhs fallbacks]
         [rhs (cons 42 fallbacks)])
    (check-equal? (truth-test (list '= lhs rhs))
                  (truth-test (list '= rhs lhs))))

  (define-syntax-rule (do op a b)
    (probe 'op 'a 'b))

  ; Let's create some names for our probe results:

  ; The result never became true:
  (define none '(#f #f #f))
  ; "lhs is not null and rhs is not null and <expr>"

  ; The result became true when either fallback was used:
  (define either '(#t #t #t))
  ; "lhs is null or rhs is null or <expr>"

  ; There is a missing case here: '(#t #t #f)
  ; This represents "either, but not both" which our current logic will never return.
  
  ; The result became true when both fallbacks were used:
  (define both '(#f #f #t))
  ; "(lhs is null and rhs is null) or <expr>"

  ; The result became true when the [lhs/rhs] fallback was used:
  (define lhs-or-both '(#t #f #t))
  (define rhs-or-both '(#f #t #t))
  ; "lhs is null or <expr>"

  ; The result became true when ONLY the [lhs/rhs] fallback was used:
  (define lhs-only '(#t #f #f))
  (define rhs-only '(#f #t #f))
  ; "(lhs is null and rhs is not null) or <expr>"


  (check-equal? (do = /void /void)
                none)
  (check-equal? (do = /minval /minval)
                both)
  (check-equal? (do = /minval /any)
                rhs-or-both)
  (check-equal? (do = /any /minval)
                lhs-or-both)
  (check-equal? (do < /minval /any)
                either)
  (check-equal? (do < /minval 42)
                lhs-or-both)
  (check-equal? (do < 42 /maxval)
                rhs-or-both)
  (check-equal? (do < /maxval 42)
                none)
  (check-equal? (do < /minval /void)
                lhs-only)
  (check-equal? (do < /minval /minval)
                lhs-only)
  (check-equal? (do < /minval /maxval)
                either)
  (check-equal? (do < /maxval /any)
                rhs-only)
  (check-equal? (do < /any /minval)
                lhs-only)

  ; What if there are no fallbacks?
  (check-equal? (do = 42 42)
                none)
  (check-equal? (do = 'foo 'bar)
                none)
  (check-equal? (do <> 42 42)
                none)
  (check-equal? (do <> 'foo 'bar)
                none))
