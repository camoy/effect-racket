#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         effect-racket
         racket/contract
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; affine box

(effect can-apply? (proc))

(define (affine-handler used-set)
  (contract-handler
   [(can-apply? proc)
    (values (not (set-member? used-set proc))
            (affine-handler (set-add used-set proc)))]))

(define affine-service
  (affine-handler (seteq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (define affine/c
    (self/c
     (λ (self)
       (->* (any/c)
            #:pre/desc
            (or (can-apply? self)
                "already applied affine function")
            any/c))))

  (chk
   #:do (define/contract (f x)
          affine/c
          x)

   (with (affine-service)
     (f 10))
   10

   #:x
   (with (affine-service)
     (f 10)
     (f 10))
   "already applied affine function"
   ))
