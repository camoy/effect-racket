#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide get
         reader
         with)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(struct reader (default))

(define-effect (get r)
  (reader-default r))

(define-syntax (with stx)
  (syntax-parse stx
    [(_ ([?r:expr ?e:expr] ...) ?b:expr)
     #'(let ([lookup (hash (~@ ?r ?e) ...)])
         (define (assigns? r)
           (hash-has-key? lookup r))
         (handle ?b
           [(get (and (? assigns?) r))
            (continue (hash-ref lookup r))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define thing (reader 0))

  (chk
   (get thing)  0
   (with ([thing 42]) (get thing))  42
   (with ([thing 42])
     (with ([thing 43])
       (get thing)))
   43
   ))
