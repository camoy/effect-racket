#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "../main.rkt"
         "../private/box.rkt"
         racket/contract
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only-mutates/c

(define (only-mutates/c b-ok)
  (define (effect-ok? e)
    (match e
      [(box-set b _) (eq? b b-ok)]
      [_ #t]))
  (->e effect-ok? any/c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:do (define/contract (f b g)
          (->i ([b box?] [g (b) (only-mutates/c b)]) any)
          (g))

   (with (box-service)
     (define b (box 0))
     (f b (λ () (box-set b 42)))
     (box-get b))
   42

   #:x
   (with (box-service)
     (define b (box 0))
     (define b2 (box 0))
     (f b (λ () (box-set b2 42))))
   "expected: effect-ok?"
   ))
