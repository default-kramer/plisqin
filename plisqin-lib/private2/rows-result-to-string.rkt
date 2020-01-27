#lang racket
(require db)

(provide rows-result->string)

(define separator "\t")

(define/contract (get-col-names result)
  (-> rows-result? (listof string?))
  (map (λ(props)
         (let ([col-name (assq 'name props)])
           ; first - column name:
           (if col-name
               (cdr col-name)
               "[no column name]")))
       (rows-result-headers result)))

(define/contract (get-rows result)
  (-> rows-result? (listof (listof string?)))
  (define rows (map vector->list (rows-result-rows result)))
  (map (λ(row) (map ~a row)) rows))

(define/contract (get-max-widths rows [maxes #f])
  (->* [(listof (listof string?))]
       [(or/c #f (listof integer?))]
       (or/c #f (listof integer?)))
  (match rows
    [(list) maxes]
    [(list row rest ...)
     (set! maxes (map (λ(str curr-max)
                        (max curr-max
                             (string-length str)))
                      row
                      (or maxes
                          (map (λ(x) 0) row))))
     (get-max-widths rest maxes)]))

(define/contract (rows-result->string result)
  (-> rows-result? string?)

  (define col-names (get-col-names result))
  (define rows (get-rows result))
  (set! rows (list* col-names
                    (map (λ(x) "---") col-names)
                    rows))
  (define max-widths (get-max-widths rows))

  (define (pad str len #:left? [left? #t])
    (define difference (- len (string-length str)))
    (if (> difference 0)
        (let ([padding (make-string difference #\space)])
          (if left?
              (string-append str padding)
              (string-append padding str)))
        str))

  (define (format-table rows)
    (define (format-row row)
      (define cells (map pad row max-widths))
      (string-join cells separator))
    (string-join (map format-row rows) "\n"))

  (format-table rows))
