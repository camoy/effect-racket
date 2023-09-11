#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [box-service handler?]
  [box (-> any/c box?)]
  [box? predicate/c])

 ;; TODO: How to get contracts on these, while still exporting
 ;; match expanders?
 box-get
 box-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; box service

(struct box (default))

(effect box-get (b))
(effect box-set (b v))

(define (make-box-service σ)
  (handler
   [(box-get b)
    (continue (hash-ref σ b (λ () (box-default b))))]
   [(box-set b v)
    (with ((make-box-service (hash-set σ b v)))
      (continue* (void)))]))

(define box-service
  (make-box-service (hasheq)))
