#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         effect-racket
         racket/contract
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; must call

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
   (with/c (h (set)))))

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
   #:do (define/contract (must-id thk)
          (-> (ensure/c another-id) any)
          (thk))

   #:x
   (must-id (λ () (+ 1 1)))
   "did not call another-id"

   (must-id (λ () (another-id 1)))
   1
   ))
