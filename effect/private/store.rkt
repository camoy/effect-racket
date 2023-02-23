#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [box (-> any/c box?)]
  [box? predicate/c]
  [box-get (-> box? any/c)]
  [box-set! (-> box? any/c void?)]
  [store? predicate/c]
  #;[current-store (-> (or/c #f store?))]
  #;[replace-store! (-> store? void?)])
 current-store
 replace-store!
 with-store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boxes

(struct box (value))

(define (box-get r)
  (hash-ref (current-store) r (λ () (box-value r))))

(define (box-set! r v)
  (replace-store! (hash-set (current-store) r v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stores

(define store? (hash/c box? any/c #:immutable #t))

(define-effect (current-store)
  (continue (hash)))

(define-effect (replace-store! s)
  (with-store-proc (hash) (λ () (continue (void)))))

(define-syntax (with-store stx)
  (syntax-parse stx
    [(_ ?store:expr ?body:expr ...+)
     #'(with-store-proc ?store (λ () ?body ...))]))

(define (with-store-proc init-sto body-thunk)
  (define run/store
    (handle (const (body-thunk))
      [(current-store)
       (λ (sto) ((continue sto) sto))]
      [(replace-store! sto*)
       (λ (sto) ((continue (void)) sto*))]))
  (run/store init-sto))
