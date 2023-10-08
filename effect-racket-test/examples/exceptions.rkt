#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         effect-racket
         racket/contract
         racket/set
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; allow exception

(effect throw (exn))

(define throw-service
  (handler
   [(throw x) (error 'throw-service "~a" x)]))

(define (throws/c s)
  (define (allowed? eff-val)
    (match eff-val
      [(throw x) (set-member? s x)]
      [_ #t]))
  (->e allowed? any/c))

(define/contract (f g)
  (throws/c (set 'foo))
  (g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk

   #:x
   (with (throw-service)
     (f (λ () (throw 'foo))))
   "throw-service: foo"

   #:x
   (with (throw-service)
     (f (λ () (throw 'bar))))
   "f: broke its own contract"
   ))
