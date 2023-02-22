#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide get
         set
         let-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state

(struct state ())

(define (bad) (error "invalid reference"))
(define-effect (get r) (bad))
(define-effect (set r v) (bad))

(define-syntax (let-ref stx)
  (syntax-parse stx
    [(_ ([?r:expr ?e:expr] ...) ?b:expr ...)
     #'(let ([?r (state)] ...)
         (define comp
           (handle (let ([result (begin ?b ...)])
                     (λ (store) result))
             [(get r)
              (λ (store)
                (unless (hash-has-key? store r) (bad))
                (define res (continue (hash-ref store r)))
                (res store))]
             [(set r v)
              (λ (store)
                (unless (hash-has-key? store r) (bad))
                ((continue (void)) (hash-set store r v)))]))
         (comp (hash (~@ ?r ?e) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   (let-ref ([r 42])
     (get r))
   42

   (let-ref ([r 42])
     (set r 10)
     (get r))
   10

   #:x (get (let-ref ([r 42]) r))
   "invalid reference"
   ))
