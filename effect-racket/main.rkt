#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 ;; `effect.rkt`
 (contract-out
  [handler? predicate/c]
  [effect-value? predicate/c]
  [handler-append (-> handler? ... handler?)])

 effect
 handler
 contract-handler
 continue
 continue*
 with

 ;; `contract.rkt`
 (contract-out
  [with/c (-> contract-handler? ... contract?)]
  [->e (-> contract? contract? contract?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/effect.rkt"
         "private/contract.rkt")
