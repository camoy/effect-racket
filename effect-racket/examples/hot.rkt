#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "../main.rkt"
         racket/contract
         racket/function
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sort

(effect sorting? ())

(define comparator/c
  (->* (any/c any/c)
       #:pre/desc
       (or (sorting? #:fail #f)
           "cannot call outside of sort")
       boolean?))

(define sorting-service
  (contract-handler
   [(sorting?) (values #t sorting-service)]))

(define sort/c
  (and/c
   (with/c
    sorting-service)
   (->* (list? comparator/c)
        #:pre/desc
        (or (not (sorting? #:fail #f))
            "not re-entrant")
        list?)))

(define isort
  (invariant-assertion
   sort/c
   (λ (xs lt?)
     (define (ins x ys)
       (match ys
         [(list) (list x)]
         [(cons y rst) #:when (lt? x y) (cons x ys)]
         [(cons y rst) (cons y (ins x rst))]))
     (foldl ins null xs))))

(define qsort
  (invariant-assertion
   sort/c
   (λ (xs lt?)
     (match xs
       ['() '()]
       [(cons pivot xt)
        (define (lt?-pivot x) (lt? x pivot))
        (append (qsort lt? (filter lt?-pivot xt))
                (list pivot)
                (qsort lt? (filter (negate lt?-pivot) xt)))]))))

(define escaped #f)
(define bad-sort
  (invariant-assertion
   sort/c
   (λ (xs lt?)
     (set! escaped lt?)
     xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   (isort '(3 2 1) <)
   '(1 2 3)

   #:x
   (qsort '(3 2 1) <)
   "not re-entrant"

   #:do (bad-sort '(3 2 1) <)
   #:x (escaped 1 2)
   "cannot call outside of sort"
   ))
