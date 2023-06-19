#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 ;; `effect.rkt`
 (contract-out
  [handler? predicate/c]
  [effect-value? predicate/c]
  [effect-value->list (-> effect-value? list?)])

 effect
 handler
 contract-handler
 continue
 with

 ;; `contract.rkt`
 (contract-out
  [contract-handler/c (-> contract-handler? any)]
  [->e (-> contract? contract? any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/effect.rkt"
         "private/contract.rkt")