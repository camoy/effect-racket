#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require effect-racket
         effect-racket/private/box
         racket/contract
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only-effects/c

(define (only-effects/c . preds)
  (define (ok? x)
    (for/or ([p (in-list preds)])
      (p x)))
  (->e ok? any/c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:do (define/contract (do thk)
          (only-effects/c
           (λ (eff-val)
             (match eff-val
               [(box-set b x) (= x 42)]
               [_ #t])))
          (thk))

   (with (box-service)
     (do (λ ()
           (define b (box 0))
           (box-set b 42)
           (box-get b))))
   42


   #:x
   (with (box-service)
     (do (λ ()
           (define b (box 0))
           (box-set b 41)
           (box-get b))))
   "promised: ok?"
   ))
