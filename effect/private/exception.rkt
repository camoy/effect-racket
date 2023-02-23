#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  #;[throw (-> any/c none/c)]
  [error (->* () #:rest error-rest? none/c)])
 throw
 catch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/format
         racket/match
         racket/string
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; throw and catch

(define-effect (throw v)
  (raise v))

(define-syntax (catch stx)
  (syntax-parse stx
    [(_ ([?pred:expr ?handler:expr] ...) ?body:expr ...+)
     #'(catch-proc (list (list ?pred ?handler) ...)
                   (λ () ?body ...))]))

(define (catch-proc handlers body-thunk)
  (handle (body-thunk)
    [(throw v)
     (define handler
       (for/first ([pred+handler (in-list handlers)]
                   #:do [(match-define (list pred handler) pred+handler)]
                   #:when (pred v))
         handler))
     ((or handler throw) v)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auxiliary

(define (error . args)
  (define marks (current-continuation-marks))
  (define str
    (match args
      [(list (? symbol? msg-sym))
       (format "error: ~a" msg-sym)]
      [(cons (? string? msg-str) vs)
       (string-join (map ~a vs))]
      [(list* (? symbol? who-sym) (? string? format-str) vs)
       (format "~a: ~a" who-sym (apply format format-str vs))]))
  (throw (exn str marks)))

(define (error-rest? args)
  (and/c list?
         pair?
         (or (symbol? (car args))
             (string? (car args)))))
