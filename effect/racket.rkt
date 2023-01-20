#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-from-out racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/transformer)
         racket/control
         racket/list
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; effect sugar

(define effect-prompt-tag
  (make-continuation-prompt-tag))

(struct effect (default)
  #:property prop:procedure
  (λ (self . args)
    (define (body kont)
      (abort/cc effect-prompt-tag (list* kont self args)))
    (call/comp/check body effect-prompt-tag self args)))

(define-syntax (define-effect stx)
  (syntax-parse stx
    [(_ (?name:id ?param:id ...) ?body:expr ...)
     #:with ?arity #`#,(length (syntax->list #'(?param ...)))
     #:do [(define effect-stx #'(effect (λ (?param ...) ?body ...)))]
     #:with ?effect-id (syntax-local-lift-expression effect-stx)
     #'(define-match-expander ?name
         (make-match-transformer #'?effect-id ?arity)
         (make-variable-like-transformer #'?effect-id))]))

(begin-for-syntax
  (define ((make-match-transformer effect-id arity) stx)
    (syntax-parse stx
      [(_ ?p ...)
       #:fail-unless (= (length (syntax->list #'(?p ...))) arity)
       "arity mismatch"
       #`(list (== #,effect-id) ?p ...)])))

(define current-continue (make-parameter #f))

;; TODO: can we guarantee that ?p is an effect pattern?
(define-syntax (handle stx)
  (syntax-parse stx
    [(_ ?e:expr [?p:expr ?v:expr ...] ...)
     #'(let ()
         (define (user-handler args)
           (match args
             [?p ?v ...] ...
             [_ (fallback args user-handler)]))
         (install-handler (λ () ?e) user-handler))]))

(define (install-handler proc user-handler)
  (define (handler vs)
    (match-define (cons kont args) vs)
    (parameterize ([current-continue (wrap kont user-handler)])
      (user-handler args)))
  (call/prompt proc effect-prompt-tag handler))

(define (wrap proc user-handler)
  (λ args
    (install-handler (λ () (apply proc args)) user-handler)))

(define (fallback args user-handler)
  (match-define (cons eff more-args) args)
  (call/comp/check
   (λ (kont)
     (define kont* (extend kont (current-continue)))
     (abort/cc effect-prompt-tag (cons kont* args)))
   effect-prompt-tag eff more-args))

(define (call/comp/check proc pt eff args)
  (if (continuation-prompt-available? pt)
      (call/comp proc pt)
      (apply (effect-default eff) args)))

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

(module+ test
  (define-effect (print-str str)
    (error 'print-str))

  (define-effect (print-num n)
    (error 'print-num))

  (handle
   (handle (let ()
             (print-str "hello")
             (print-num 42)
             (print-str "world")
             (print-num 43))
           [(print-str str)
            (displayln str)
            (continue (void))])
   [(print-num n)
    (displayln (format "#~a" n))
    (continue (void))]))
