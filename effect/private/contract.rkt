#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 contract-handler/c
 ->e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/unsafe/ops
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `contract-handler/c`

(struct contract-handler/c (handler)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (self) '(contract-handler/c ???))
   #:late-neg-projection
   (λ (self)
     (match-define (contract-handler/c handler) self)
     (λ (blm)
       (λ (proc neg)
         (unsafe-chaperone-procedure
          proc
          (make-keyword-procedure
           (λ (kws kw-args . args)
             (with (handler)
               (keyword-apply proc kws kw-args args)))
           (λ args
             (with (handler)
               (apply proc args))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `->e`

(define (->e to from)
  (->e-contract
   (coerce-contract '->e to)
   (coerce-contract '->e from)))

(struct ->e-contract (to from)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (self)
     (match-define (->e-contract to from) self)
     `(->e ,(contract-name to) ,(contract-name from)))
   #:late-neg-projection
   (λ (self)
     (match-define (->e-contract to from) self)
     (define to-lnp (get/build-late-neg-projection to))
     (define from-lnp (get/build-late-neg-projection from))
     (λ (blm)
       (define to-blm (blame-add-context blm "the performed effect"))
       (define to-lnp+blm (to-lnp to-blm))
       (define from-blm (blame-add-context blm "the effect response" #:swap? #t))
       (define from-lnp+blm (from-lnp from-blm))
       (λ (proc neg)
         (define (perform* eff)
           (define arg
             (with-contract-continuation-mark
               (cons to-blm neg)
               (to-lnp+blm eff neg)))
           (define res (perform arg ABSENT))
           (with-contract-continuation-mark
             (cons from-blm neg)
             (from-lnp+blm res neg)))
         (define h (handler [eff (continue (perform* eff))]))
         (unsafe-chaperone-procedure
          proc
          (make-keyword-procedure
           (λ (kws kw-args . args)
             (with (h)
               (keyword-apply proc kws kw-args args)))
           (λ args
             (with (h)
               (apply proc args))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (effect other ())
  (effect generating ())

  (define (generating-handler val)
    (contract-handler
     [(generating)
      (values val (generating-handler val))]))

  (define generator/c
    (-> (contract-handler/c (generating-handler #t))
        void?))

  (define yield/c
    (->i ([x any/c])
         #:pre/name ()
         "must be generating"
         (generating)
         [result any/c]))

  (chk
   ;; `contract-handler/c`
   #:do (define/contract (generator f)
          generator/c
          (f)
          (void))

   #:do (define/contract (yield x)
          yield/c
          x)

   (with ((generating-handler #f))
     (generator void))
   (void)

   (with ((generating-handler #f))
     (generator (λ () (yield 42))))
   (void)

   #:x
   (with ((generating-handler #f))
     (yield 42))
   "must be generating"

   ;; `->e`
   #:do (define/contract (g f)
          (-> (->e generating? boolean?) any)
          (f))

   #:do (define/contract (h f)
          (-> (->e any/c number?) any)
          (f))

   #:do (define/contract (i f)
          (-> (->e other? boolean?) any)
          (f))

   #:do (define/contract (j)
          (->e (λ (x) (generating) (generating))
               (λ (x) (generating)))
          (generating))

   #:do
   (define (generating-handler* val)
     (handler
      [(generating) (continue val)]))

   (with ((generating-handler* 17)
          (generating-handler #t))
     (j))
   17

   #:t
   (with ((generating-handler* #t))
     (g (λ () (generating))))

   #:x
   (with ((generating-handler* #t))
     (h (λ () (generating))))
   "promised: number?"

   #:x
   (with ((generating-handler* #t))
     (i (λ () (generating))))
   "expected: other?"
   ))
