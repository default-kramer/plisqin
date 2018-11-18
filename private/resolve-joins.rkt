#lang racket
(require "core.rkt" "util.rkt")
(module+ test
  (require rackunit)
  (require "macros.rkt")
  (define (src alias table uid)
    (s:source alias table uid)))

(provide resolve-joins)

; The basic algorithm is:
; Do for each query/join, from most-nested to least-nested (root query):
;   Collect and de-duplicate joins in the immediate scope (no child scopes).
;   NOTE that "join on" clauses actually belong to the parent scope.
;   Do this for the immediate scope AND child scopes:
;     Replace inline joins with the canonical source.
;     Replace sources with the canonical source.
;   Set the query's list of joins to be the collected-and-deduplicated list.


; Let's invent 2 concepts: the "normal form" and the "canonical version".
; The implementation and explanation of "normal form" has been moved to core.rkt.
; The "canonical version" of a Join is simply "the first one we enountered amongst all equivalent joins."
(define normal-source? source?)
(define canonical-source? source?)
(define normal-join? join?)
(define canonical-join? join?)

(module+ test
  (check-equal?
   (normalize `(,(source "X" "x" #:uid 234)
                "blah"
                ,(source "X" "x" #:uid 234)
                ,(source "X" "x" #:uid 654)))
   `(,(source "normal" "x" #:uid 1)
     "blah"
     ,(source "normal" "x" #:uid 1)
     ,(source "normal" "x" #:uid 2)))

  (let ([j1 (join j1 "J"
                  (join-on j1".X > 3"))]
        [j2 (join j2 "J"
                  (join-on j2".X > 3"))])
    (begin
      (check-not-equal? j1 j2)
      (check-equal? (normalize j1) (normalize j2)))))


; joinmap (Immutable-HashTable NormalJoin CanonicalJoin)
; sourcemap (Immutable-HashTable Source CanonicalSource)
; ordered-joins (Listof CanonicalJoin)
(struct join-collector (joinmap sourcemap ordered-joins) #:transparent)

(define (new-join-collector)
  (join-collector (make-immutable-hash) (make-immutable-hash) (list)))

; Given a join, visit the join-on clauses and replace inline joins with canonical sources.
(define/contract (deinline-clauses jc j)
  (-> join-collector? join? join?)
  (define (visit node)
    (let ([sourcemap (join-collector-sourcemap jc)])
      (if (and (join? node)
               (hash-has-key? sourcemap (get-src node)))
          (return (hash-ref sourcemap (get-src node)))
          (return node))))
  (let* ([clauses (s:join-clauses j)]
         [clauses (walk clauses #:on-enter visit)])
    (struct-copy s:join j
                 [clauses clauses])))

(define/contract (add-join jc orig-join)
  (-> join-collector? join? join-collector?)
  (let* ([normal-join (normalize orig-join)]
         ; If we've never seen this join before, it becomes the canonical version.
         [joinmap (join-collector-joinmap jc)]
         [seen-before (hash-has-key? joinmap normal-join)]
         [joinmap (if seen-before
                      joinmap ; no change, else
                      ; add mapping from normal to de-inlined original
                      (hash-set joinmap normal-join (deinline-clauses jc orig-join)))]
         [canonical-join (hash-ref joinmap normal-join)]
         ; Add a source mapping
         [sourcemap (join-collector-sourcemap jc)]
         [sourcemap (hash-set sourcemap
                              (get-src orig-join)
                              (get-src canonical-join))]
         ; Update list of ordered-joins
         [ordered-joins (join-collector-ordered-joins jc)]
         [ordered-joins (if seen-before
                            ordered-joins ; no change
                            (cons canonical-join ordered-joins))])
    (struct-copy join-collector jc
                 [joinmap joinmap]
                 [sourcemap sourcemap]
                 [ordered-joins ordered-joins])))

(module+ test
  (let ()
    (define root (source "r" "Root"))
    (define parent (source "p" "Parent"))
    (define grandparent (source "gp" "Parent"))
    (define example (from _ root
                          (join _ grandparent
                                (join-on grandparent".ID = "
                                         (join _ parent
                                               (join-on parent".ID = "root".PARENT_ID"))
                                         ".PARENT_ID"))))
    (define jc (new-join-collector))
    ; processing inside-out, we should see the "parent" join first
    (set! jc (add-join jc (join _ parent
                                (join-on parent".ID = "root".PARENT_ID"))))
    ; then see the "grandparent" with the "parent" join inline
    (set! jc (add-join jc (join _ grandparent
                                (join-on grandparent".ID = "
                                         (join _ parent
                                               (join-on parent".ID = "root".PARENT_ID"))
                                         ".PARENT_ID"))))
    ; But we should have de-inlined "parent" out of "grandparent" when we canonicalized "grandparent"
    (check-equal?
     (first (join-collector-ordered-joins jc))
     (join _ grandparent
           (join-on grandparent".ID = "parent".PARENT_ID")))))

; Collect joins only in the immediate scope - subqueries are not searched
(define/contract (collect-joins q)
  (-> query? join-collector?)
  (define (stop-on-query node)
    (if (or (query? node)
            ; Bindings should be skipped also. If a binding comes from a parent query we are supposed
            ; to ignore it (trust that it gets joined there). If a binding comes from the current
            ; query then the join should be the query's list of joins anyway, so we'll collect it then.
            (binding? node))
        (return (no-recurse node))
        (return node)))
  ; When we see a join, add it to the join collector  
  (define (visit node jc)
    (cond
      [(join? node) (return node (add-join jc node))]
      [else (return node)]))
  ; Main body - walk the explicit joins first, then search the fragments
  (let* ([jc (new-join-collector)]
         [result (walk (s:query-joins q) #:on-enter stop-on-query #:on-exit visit #:accum jc)]
         [jc (car result)]
         [result (walk (query-fragments q) #:on-enter stop-on-query #:on-exit visit #:accum jc)]
         [jc (car result)])
    jc))

(module+ test
  (let ()
    (define root (source "r" "Root"))
    (define parent (source "p" "Parent"))
    (define grandparent (source "gp" "Parent"))
    (define example (from _ root
                          (join _ grandparent
                                (join-on grandparent".ID = "
                                         (join _ parent
                                               (join-on parent".ID = "root".PARENT_ID"))
                                         ".PARENT_ID"))
                          ; Let's put a join in a subquery to prove it doesn't get collected
                          (where (exists (from subquery "Sub"
                                               (join x "X" (join-on "1=1")))))))
    (define jc (collect-joins example))
    (define joins (join-collector-ordered-joins jc))
    (check-equal? (length joins) 2)
    ; We should have de-inlined the "parent" out of the "grandparent"
    (check-equal?
     (first joins)
     (join _ grandparent
           (join-on grandparent".ID = "parent".PARENT_ID")))
    (check-equal?
     (second joins)
     (join _ parent
           (join-on parent".ID = "root".PARENT_ID")))))

(define/contract (get-replacement jc node)
  (-> join-collector? any/c any/c)
  (if (or (join? node)
          (source? node))
      (let ([source (get-src node)]
            [sourcemap (join-collector-sourcemap jc)])
        (hash-ref sourcemap source node))
      node))

; Once we have collected the joins into a join-collector, we can reconstruct
; the query with simple sources in place of joins. Note that we do this even
; into child subqueries, as a join that was mentioned on the root query might
; be accessed by a subquery. This does not modify the join-collector at all.
(define/contract (apply-jc2 jc q)
  (-> join-collector? query? query?)
  ; When we see a query, explicitly recurse to avoid clobbering its list of joins.
  ; We we see a join or a source, replace it with the canonical version.
  (define (visit node)
    (cond [(query? node)
           (return (no-recurse (apply-jc2 jc node)))]
          [else (return (get-replacement jc node))]))
  ; Main body
  (let ([new-frags (walk (query-fragments q) #:on-enter visit)])
    (struct-copy s:query q
                 [clauses new-frags])))
; On the root query only, the collected canonical joins replace the list of joins
(define/contract (apply-jc jc q)
  (-> join-collector? query? query?)
  (struct-copy s:query (apply-jc2 jc q)
               [joins (reverse (join-collector-ordered-joins jc))]))

; The main algorithm. Work from innermost query to outermost.
; Collect joins, then apply them. Simple, right?
(define/contract (resolve-joins q)
  (-> query? query?)
  (define (visit node)
    (cond [(query? node)
           (let* ([jc (collect-joins node)]
                  [node (apply-jc jc node)])
             (return node))]
          [else (return node)]))
  (walk q #:on-exit visit))

(module+ test
  (let ()
    (define (Title-of/s rating)
      (join t "Title"
            (join-on t".TitleID = "rating".TitleID")))
    (define q (from r "Rating"
                    (where (Title-of/s r)".StartYear >= 1990")
                    (where (Title-of/s r)".EndYear < 2000")))
    (void (resolve-joins q))))