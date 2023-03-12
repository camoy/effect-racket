#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (rename-out [mb #%module-begin])
 (except-out
  (all-from-out racket/base)
  ;; #%module-begin
  #%module-begin

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
 (all-from-out "private/contract.rkt"
               "private/effect.rkt"
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
                      "private/contract.rkt"
                      "private/exception.rkt"
                      "private/io.rkt"
                      "private/store.rkt")
         racket/block
         racket/control
         syntax/wrap-modbeg
         "private/contract.rkt"
         "private/effect.rkt"
         "private/exception.rkt"
         "private/io.rkt"
         "private/store.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; module-begin

(define-syntax mb (make-wrapping-module-begin	#'print-result))

(define-syntax (print-result stx)
  (syntax-parse stx
    [(_ ?e)
     #'(call/prompt
        (λ ()
          (call-with-values (λ () ?e) print-values)))]))

(define (print-values . vs)
  (for ([v (in-list vs)]
        #:unless (void? v))
    (println v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  effect/racket)
