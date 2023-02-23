#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (except-out
  (all-from-out racket/base)
  ;; set!
  set! set!-values

  ;; boxes
  box-immutable unbox set-box! unbox* set-box*! box-cas!

  ;; errors
  raise-user-error
  raise-argument-error raise-argument-error*
  raise-result-error raise-result-error*
  raise-arguments-error raise-arguments-error*
  raise-range-error raise-range-error*
  raise-type-error
  raise-mismatch-error
  raise-arity-error raise-arity-error*
  raise-arity-mask-error raise-arity-mask-error*
  raise-result-arity-error raise-result-arity-error*
  raise-syntax-error)
 (all-from-out "private/effect.rkt"
               "private/exception.rkt"
               "private/io.rkt"
               "private/store.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/require)
(require (prefix-in base: racket/base)
         (for-syntax racket/base
                     syntax/parse)
         (subtract-in racket/base
                      "private/exception.rkt"
                      "private/io.rkt"
                      "private/store.rkt")
         racket/block
         "private/effect.rkt"
         "private/exception.rkt"
         "private/io.rkt"
         "private/store.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  effect/racket)
