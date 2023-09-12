#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 ;; `effect.rkt`
 (contract-out
  [handler? predicate/c]
  [effect-value? predicate/c])

 effect
 handler
 contract-handler
 continue
 continue*
 with

 ;; `contract.rkt`
 (contract-out
  [with/c (-> contract-handler? any)]
  [->e (-> contract? contract? any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/effect.rkt"
         "private/contract.rkt")
