#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "../main.rkt"
         contract-etc
         racket/contract
         racket/function
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prohibit

(effect allowed? (proc))

(define (prohibit/c x)
  (define h
    (contract-handler
     [(allowed? (== x)) (values #f h)]))
  (contract-handler/c h))

(define (=> dom/c cod/c)
  (self/c
   (λ (this)
     (->* (dom/c)
          #:pre/desc
          (or (allowed? this #:fail (const #t))
              (format "cannot call ~a" (object-name this)))
          cod/c))))

(define/contract (id x)
  (=> any/c any/c)
  x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ensure

(effect called? (proc))
(effect call! (proc))

(define (ensure/c x)
  (define (h s)
    (contract-handler
     [(called? x) (values (set-member? s x) (h s))]
     [(call! x) (values (void) (h (set-add s x)))]))
  (and/c
   (->* () any/c
        #:post/desc
        (or (called? x)
            (format "did not call ~a" (object-name x))))
   (contract-handler/c (h (set)))))

(define (~> dom/c cod/c)
  (self/c
   (λ (this)
     (->* (dom/c)
          #:pre (call! this)
          cod/c))))

(define/contract (another-id x)
  (~> any/c any/c)
  x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define/contract (no-id thk)
          (-> (prohibit/c id) any)
          (thk))

   (no-id (λ () (+ 1 1)))
   2

   #:x (no-id (λ () (id 1)))
   "cannot call id"

   #:do (define/contract (must-id thk)
          (-> (ensure/c another-id) any)
          (thk))

   #:x
   (must-id (λ () (+ 1 1)))
   "did not call another-id"

   (must-id (λ () (another-id 1)))
   1
   ))
