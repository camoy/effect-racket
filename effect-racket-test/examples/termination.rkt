#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide total->)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         contract-etc
         effect-racket
         racket/bool
         racket/contract
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct edge (left change right) #:transparent)

(define (down? e) (eq? (edge-change e) '↓))
(define (same? e) (eq? (edge-change e) '↧))
(define (cons* a d) (cons d a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract parameter

(effect history-get ())

(define (history-handler . params)
  (contract-handler
   [(history-get)
    (values (if (history-get #:fail #f)
                (update (history-get) params)
                (history params null))
            (apply history-handler params))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract

(struct history (prior graphs) #:transparent)

(define-syntax (total-> stx)
  (syntax-parse stx
    [(_ arg-ctc ... res-ctc)
     #:with (param ...) (generate-temporaries #'(arg-ctc ...))
     #'(and/c
        (dynamic->d
         (λ (param ...)
           (with/c
            (history-handler param ...))))
        (->i ([param arg-ctc] ...)
             #:pre (param ...)
             (or (not (history-get #:fail #f))
                 (update (history-get) (list param ...)))
             [result res-ctc]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update

(define (update hist args2)
  (match-define (history args1 gs) hist)
  (define gn (graph args1 args2))
  (and (prog? gn gs)
       (history args2 (cons gn gs))))

(define (graph args1 args2)
  (for*/fold ([s (set)])
             ([(a1 i) (in-indexed (in-list args1))]
              [(a2 j) (in-indexed (in-list args2))])
    (cond
      [(lt a2 a1) (set-add s (edge i '↓ j))]
      [(equal? a2 a1) (set-add s (edge i '↧ j))]
      [else s])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; progress

(define (prog? gn gs)
  (for/fold ([pre gn]
             [ok? (desc? gn)]
             #:result ok?)
            ([g (in-list gs)])
    #:break (not ok?)
    (define pre* (comp pre g))
    (values pre* (desc? pre*))))

(define (desc? g)
  (implies (idempotent? g) (ormap down? (set->list g))))

(define (idempotent? g)
  (equal? (comp g g) g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; composition

(define (comp g1 g2)
  (define h1 (edge-hash g1 edge-left edge-right))
  (define h2 (edge-hash g2 edge-right edge-left))
  (define downs
    (set-union
     (down-cons-set h1 g2 cons edge-left edge-right)
     (down-cons-set h2 g1 cons* edge-right edge-left)))
  (define sames
    (set-subtract
     (same-cons-set h1 h2)
     downs))
  (set-union
   (cons-set->edges downs '↓)
   (cons-set->edges sames '↧)))

(define (down-cons-set h1 g2 kons outer inner)
  (for/fold ([g (set)])
            ([e (in-set g2)])
    (define is (hash-ref h1 (outer e) null))
    (define k (inner e))
    (define g*
      (for/set ([i (in-list is)])
        (kons i k)))
    (set-union g g*)))

(define (same-cons-set h1 h2)
  (for*/set ([(j is) (in-hash h1)]
             [k (in-list (hash-ref h2 j null))]
             [i (in-list is)])
    (cons i k)))

(define (edge-hash g outer inner [down? down?])
  (for/fold ([ht (hash)])
            ([e (in-set g)] #:when (down? e))
    (hash-update ht
                 (inner e)
                 (λ (is) (cons (outer e) is))
                 null)))

(define (cons-set->edges s c)
  (for/set ([p (in-set s)])
    (match-define (cons a d) p)
    (edge a c d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comparator

(define (lt x y)
  (match* (x y)
    [((? number?) (? number?)) (< x y)]
    [(_ (cons a d)) (or (leq x a) (leq x d))]
    [(_ _) #f]))

(define (leq x y)
  (or (lt x y)
      (equal? x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define (hash-map-values f ht)
    (for/hash ([(k v) (in-hash ht)])
      (values k (f v))))

  (chk
   ;; comparator (numbers)
   #:t (lt 1 2)
   #:! #:t (lt 2 1)
   #:! #:t (lt "1" 1)
   #:t (leq 1 2)
   #:t (leq 1 1)
   #:! #:t (leq 2 1)
   #:! #:t (leq "1" 1)

   ;; comparator (pairs)
   #:t (lt 1 '(1 2))
   #:! #:t (lt '(1 2) 1)
   #:t (lt '(2) '(1 2))
   #:t (leq '(2) '(1 2))
   #:t (leq '(1 2) '(1 2))

   ;; edge hash
   (hash-map-values
    list->set
    (edge-hash
     (set (edge 1 '↓ 2)
          (edge 2 '↓ 2)
          (edge 1 '↓ 3))
     edge-left
     edge-right))

   (hash
    2 (set 1 2)
    3 (set 1))

   (hash-map-values
    list->set
    (edge-hash
     (set (edge 1 '↓ 2)
          (edge 2 '↓ 2)
          (edge 1 '↓ 3))
     edge-right
     edge-left))

   (hash
    1 (set 2 3)
    2 (set 2))

   ;; down-cons-set
   (down-cons-set
    (edge-hash
     (set (edge 1 '↓ 2)
          (edge 2 '↓ 2)
          (edge 1 '↓ 3))
     edge-left
     edge-right)
    (set (edge 2 '↧ 4)
         (edge 1 '↓ 3))
    cons edge-left edge-right)

   (set '(1 . 4) '(2 . 4))

   (down-cons-set
    (edge-hash
     (set (edge 2 '↧ 4)
          (edge 1 '↓ 3))
     edge-right
     edge-left)
    (set (edge 1 '↓ 2)
         (edge 2 '↓ 2)
         (edge 1 '↓ 1))
    cons* edge-right edge-left)

   (set '(1 . 3))

   ;; same-cons-set
   (same-cons-set
    (edge-hash
     (set (edge 2 '↧ 4)
          (edge 3 '↧ 4))
     edge-left
     edge-right
     same?)
    (edge-hash
     (set (edge 4 '↧ 7)
          (edge 4 '↧ 3))
     edge-right
     edge-left
     same?))

   (set '(2 . 7)
        '(2 . 3)
        '(3 . 7)
        '(3 . 3))

   (comp (set (edge 1 '↓ 1)
              (edge 1 '↧ 1))
         (set (edge 1 '↧ 1)
              (edge 2 '↓ 2)))

   (set (edge 1 '↓ 1))

   ;; idempotent
   #:t (idempotent? (set (edge 1 '↓ 1)))
   #:! #:t (idempotent? (set (edge 1 '↓ 1) (edge 1 '↧ 2)))

   ;; graph
   (graph '(2 0) '(1 1))
   (set (edge 0 '↓ 0) (edge 0 '↓ 1))

   ;; contract
   #:do
   (define ack
     (invariant-assertion
      (total-> integer? integer? integer?)
      (λ (m n)
        (cond
          [(= 0 m) (+ 1 n)]
          [(= 0 n) (ack (- m 1) 1)]
          [else (ack (- m 1) (ack m (- n 1)))]))))

   (ack 2 0)  3
   (ack 2 2)  7

   #:do
   (define bad-ack
     (invariant-assertion
      (total-> integer? integer? integer?)
      (λ (m n)
        (cond
          [(= 0 m) (+ 1 n)]
          [(= 0 n) (bad-ack (- m 1) 1)]
          [else (bad-ack m (bad-ack m (- n 1)))]))))

   #:x (bad-ack 2 0)
   "assertion violation"
   ))
