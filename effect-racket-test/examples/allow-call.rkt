#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [generator
   (-> generating/c base:generator?)]
  [yield
   (->* ()
        #:rest list?
        #:pre/desc (or (generating? #:fail #f) "cannot call yield")
        any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in base: racket/generator)
         effect-racket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(effect generating? ())

(define generating/c
  (letrec ([generating-handler
            (contract-handler
             [(generating?)
              (values #t generating-handler)])])
    (with/c generating-handler)))

(define (generator proc)
  (base:generator () (proc)))

(define yield base:yield)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (submod "..")
           chk)

  (chk
   #:do
   (define g
     (generator
      (λ ()
        (let go ([x '(a b c)])
          (if (null? x)
              0
              (begin
                (yield (car x))
                (go (cdr x))))))))

   (list (g) (g) (g) (g) (g))
   '(a b c 0 0)

   #:x (yield 10)
   "cannot call yield"
   ))
