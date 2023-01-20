#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-from-out racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/control
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; effect sugar

(define effect-pt
  (make-continuation-prompt-tag))

(struct effect-s ())

(define-syntax effect?
  (make-rename-transformer #'effect-s?))

(define-syntax (effect stx)
  (syntax-parse stx
    [(_ ?name:id (?field:id ...))
     #'(struct ?name effect-s (?field ...))]))

(define (perform e)
  (call/comp (λ (kont) (abort/cc effect-pt e kont)) effect-pt))

(define current-continue (make-parameter #f))

(define-syntax (handle stx)
  (syntax-parse stx
    [(_ ?e:expr [?p:expr ?v:expr ...] ...)
     #'(let ()
         (define (user-handler eff)
           (match eff
             [?p ?v ...] ...
             [_ (fallback eff user-handler)]))
         (install-handler (λ () ?e) user-handler))]))

(define (install-handler proc user-handler)
  (define (handler e kont)
    (parameterize ([current-continue (wrap kont user-handler)])
      (user-handler e)))
  (call/prompt proc effect-pt handler))

(define (wrap proc user-handler)
  (λ args
    (install-handler (λ () (apply proc args)) user-handler)))

(define (fallback eff user-handler)
  (call/comp
   (λ (kont)
     (abort/cc effect-pt eff (extend kont (current-continue))))
   effect-pt))

(define (extend k1 k2)
  (λ args (call-in-continuation k1 (λ () (apply k2 args)))))

(define (continue . args)
  (apply (current-continue) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  effect/racket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; TODO: define-effect with function call-like syntax (is default handler)
;; effects are not first-class objects
;; no perform: (print-str ...) will perform (like normal function call)
;; handle does static arity checking

(module+ test
  (effect print-str (str))
  (effect print-num (n))
  (handle
   (handle (let ()
             (perform (print-str "hello"))
             (perform (print-num 42))
             (perform (print-str "world"))
             (perform (print-num 43)))
           [(print-str str)
            (displayln str)
            (continue (void))])
   [(print-num n)
    (displayln (format "#~a" n))
    (continue (void))]))
