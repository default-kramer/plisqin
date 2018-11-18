#lang at-exp racket
(provide deduplicate)
(require "core.rkt" "util.rkt")

(define-syntax-rule (discard x ...) (void))

@discard{
 What should deduplicate do?
 In short, it should make things identical if they are "pretty much equal".
 So what does it mean...?
 Joins that are identical except their UIDs should be the same.
 Same goes for injections.
 (And queries? Probably no need... why would we ever de-duplicate a subquery?)

 When we see a join or injection, replace it with a "normal version"...
 ON EXIT right? So if we have this
 @(define ex1
    (from x "X"
          (inject [i1 (join y1 "Y")] "count(*)")
          (inject [i2 (join y2 "Y")] "count(*)")))
 We will see and normalize in this order:
 -- @(join y1 "Y")
 -- @(inject [i1 (normalized (join y1 "Y"))] "count(*)")
 -- @(join y2 "Y")
 -- @(inject [i2 (normalized (join y2 "Y"))] "count(*)")
 We should be able to eventually recognize that these are identical
 -- @(inject [__I__ (join __Y__ "Y")] "count(*)")
}

(define/contract (scope-src x)
  (-> any/c (or/c source? #f))
  ; Returns the source that this node introduces, if any.
  (cond
    [(or (join? x)
         (query? x))
     (get-src x)]
    [(injection? x) (s:injection-placeholder x)]
    [else #f]))

(define (is-joined-query ancestry)
  (-> list? boolean?)
  (and ((length ancestry) . >= . 2)
       (query? (first ancestry))
       (join? (second ancestry))
       #t))

(define/contract (update-maxmap hash items level)
  (-> hash? list? number? hash?)
  (match items
    [(list) hash]
    [(list first rest ...)
     #:when (scope-src first)
     (let* ([first (scope-src first)]
            [curr-level (hash-ref hash first -1)]
            [hash (if (level . <= . curr-level)
                      hash
                      (hash-set hash first level))])
       (update-maxmap hash rest level))]
    [(list first rest ...)
     (update-maxmap hash rest level)]))

; maxmap maps (source => max level of children)
; levelmap maps (source => level)
(struct resourcer (level maxmap levelmap) #:transparent)
(define/contract (push r)
  (-> resourcer? resourcer?)
  (struct-copy resourcer r
               [level (add1 (resourcer-level r))]))
(define/contract (pop r ancestry)
  (-> resourcer? list? resourcer?)
  (struct-copy resourcer r
               [level (sub1 (resourcer-level r))]
               [maxmap (update-maxmap (resourcer-maxmap r) ancestry (resourcer-level r))]
               [levelmap (hash-set (resourcer-levelmap r)
                                   (scope-src (first ancestry))
                                   (resourcer-level r))]))
(define (empty-resourcer)
  (resourcer 0 (make-immutable-hash) (make-immutable-hash)))
(define/contract (get-normal-source r src)
  (-> resourcer? source? source?)
  (let ([level (hash-ref (resourcer-levelmap r) src #f)]
        [max (hash-ref (resourcer-maxmap r) src #f)])
    (if (and level max)
        (struct-copy s:source src
                     [uid (format "N~a" (max . - . level))])
        src)))

(define/contract (build-resourcer x)
  (-> any/c resourcer?)
  (define (enter node accum ancestry)
    (cond
      [(is-joined-query ancestry)
       (return node)]
      [(scope-src node)
       (return node (push accum))]
      [else (return node)]))
  (define (exit node accum ancestry)
    (cond
      [(is-joined-query ancestry)
       (return node)]
      [(scope-src node)
       (return node (pop accum ancestry))]
      [else (return node)]))
  (walk x #:on-enter enter #:on-exit exit #:accum (empty-resourcer) #:return 'accum))

(define/contract (apply-resourcer r x)
  (-> resourcer? any/c any/c)
  (define (enter node)
    (cond
      [(source? node)
       (return (get-normal-source r node))]
      [else (return node)]))
  (walk x #:on-enter enter))

; Deduplicate makes identical those things that are almost identical.
; (Where "almost identical" means "identical except for the UIDs of the sources".)
(define (deduplicate x)
  (apply-resourcer (build-resourcer x) x))

(module+ test
  (require rackunit "macros.rkt")

  (define (NORM table level [alias #f])
    (struct-copy s:source
                 (source (or alias (string-downcase table))
                         table)
                 [uid (format "N~a" level)])))

(define-syntax-rule (test FORMS ...)
  (module+ test
    (let ([a 0])
      FORMS
      ...)))

(test
 (define q
   (from x "X"
         (join y "Y")
         (join a "A"
               (join b "B"
                     (join-on b".blah = "y".blah")))))
 (define r (build-resourcer q))
 (define expected
   (from x (NORM "X" 3)
         (join y (NORM "Y" 0))
         (join a (NORM "A" 2)
               (join b (NORM "B" 1)
                     (join-on b".blah = "y".blah")))))
 (check-equal?
  (apply-resourcer r q)
  expected))
(test
 (define q
   (from x "X"
         (join a "A"
               (join b "B"))
         (join y "Y")))
 (define r (build-resourcer q))
 (define expected
   (from x (NORM "X" 2)
         (join a (NORM "A" 1)
               (join b (NORM "B" 0)))
         (join y (NORM "Y" 0))))
 (check-equal?
  (apply-resourcer r q)
  expected))