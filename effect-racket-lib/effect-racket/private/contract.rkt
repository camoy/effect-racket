#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 dependent->e
 ->e
 with/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/function
         racket/match
         racket/unsafe/ops
         syntax/location
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `->e`

(define-syntax dependent->e
  (syntax-parser
    [(_ to make-from)
     #:declare to (expr/c #'contract?)
     #:declare make-from (expr/c #'(-> any/c contract?))
     #'(->e-contract
        (coerce-contract 'dependent->e to.c)
        (compose (curry coerce-contract 'dependent->e) make-from.c)
        (quote-module-name))]))

(define (->e to from)
  (->e-contract
   (coerce-contract '->e to)
   (λ _ (coerce-contract '->e from))
   #f))

(struct ->e-contract (to make-from indy-party)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (self)
     (match-define (->e-contract to make-from _) self)
     `(->e ,(contract-name to) ,(object-name make-from)))
   #:late-neg-projection
   (λ (self)
     (match-define (->e-contract to make-from indy-party) self)
     (define to-lnp (get/build-late-neg-projection to))
     (λ (blm)
       (define to-blm (blame-add-context blm "the performed effect"))
       (define to-lnp+blm (to-lnp to-blm))
       (define from-blm (blame-add-context blm "the effect response" #:swap? #t))
       (λ (proc neg)
         (define (perform* eff)
           (define arg
             (with-contract-continuation-mark
               (cons to-blm neg)
               (to-lnp+blm eff neg)))
           (define arg-indy
             (with-contract-continuation-mark
               (cons to-blm neg)
               (to-lnp+blm eff indy-party)))
           (define from-lnp (get/build-late-neg-projection (make-from arg-indy)))
           (define from-lnp+blm (from-lnp from-blm))
           (define res (perform arg))
           (with-contract-continuation-mark
             (cons from-blm neg)
             (from-lnp+blm res neg)))
         (define h
           (handler
            [(return results ...) (apply values results)]
            [eff (continue (perform* eff))]))
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
;; `with/c`

(define (with/c . handlers)
  (with/c-contract (apply handler-append handlers)))

(struct with/c-contract (handler)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (self) 'with/c)
   #:late-neg-projection
   (λ (self)
     (match-define (with/c-contract handler) self)
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
