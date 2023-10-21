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
 return
 with

 ;; `contract.rkt`
 dependent->e
 (contract-out
  [->e (-> contract? contract? contract?)]
  [with/c (-> contract-handler? ... contract?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/effect.rkt"
         "private/contract.rkt")
