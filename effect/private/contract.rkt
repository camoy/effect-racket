#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide handle/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/unsafe/ops
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct handler-contract (arrow install)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (self)
     (define arrow (handler-contract-arrow self))
     `(handle/c ,arrow ???))
   #:late-neg-projection
   (λ (self)
     (match-define (handler-contract arrow install) self)
     (define arrow-lnp (get/build-late-neg-projection arrow))
     (λ (blm)
       (define blm* (blame-add-context blm "arrow contract of"))
       (define arrow+blm (arrow-lnp blm*))
       (λ (proc neg)
         (unsafe-chaperone-procedure
          (arrow+blm proc neg)
          (make-keyword-procedure
           (λ (kws kw-args . args)
             (install (λ () (keyword-apply proc kws kw-args args))))
           (λ args
             (install (λ () (apply proc args)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `handle/c`

(define-syntax (handle/c stx)
  (syntax-parse stx
    [(_ ?ctc:expr [?p:expr ?e:expr] ...)
     #'(handler-contract
        ?ctc
        (λ (proc)
          (handle (proc)
            [?p ?e] ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           "store.rkt")

  (define-effect (generating?)
    (continue #f))

  (define generator/c
    (-> (handle/c (-> any/c)
          [(generating?) (continue #t)])
        void?))
  (define yield/c
    (->i ([x any/c])
         #:pre/name ()
         "must be generating"
         (generating?)
         [result any/c]))

  (chk
   #:do (define/contract (generator f)
          generator/c
          (f)
          (void))

   #:do (define/contract (yield x)
          yield/c
          x)

   (generator void)  (void)
   (generator (λ () (yield 42)))  (void)
   #:x (yield 42)
   "must be generating"
   ))
