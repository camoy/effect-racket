#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base)
           chk
           racket/runtime-path
           syntax/macro-testing)

  (module b effect/racket
    (provide inner-proc
             inner-macro)

    (define (inner-proc) 10)
    (define-syntax-rule (inner-macro x)
      'x))

  (module a effect/racket
    (require (submod ".." b)
             racket/function
             racket/math)

    (provide f
             inner-proc-ok?
             inner-macro-ok?
             sqr
             boo
             thunk)

    (define-syntax-rule (boo)
      (void))

    (define (f) #f)

    (define inner-proc-ok?
      (= (inner-proc) 10))

    (define inner-macro-ok?
      (symbol? (inner-macro good))))

  (module c effect/racket
    (require racket/math)
    (sqr 1))

  (module d effect/racket
    (provide (for-syntax b))
    (require (for-syntax racket/base))

    (begin-for-syntax
      (define (b)
        (void))))

  (define-runtime-module-path-index mod-c
    '(submod "." c))

  (define-runtime-module-path-index mod-d
    '(submod "." d))

  ;; https://stackoverflow.com/questions/55542698/dynamically-require-a-phase-1-for-syntax-variable-in-racket
  (define (dynamic-require-from-syntax module binding)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'racket)
      (namespace-require/expansion-time module)
      (eval `(define-syntax (cheater-x stx)
               #`'#,(datum->syntax #f ,binding)))
      (eval 'cheater-x)))

  (require 'a)

  (chk
   ;; effect/racket -> effect/racket (procedure)
   #:t inner-proc-ok?

   ;; effect/racket -> effect/racket (macro)
   #:t inner-macro-ok?

   ;; racket/base -> effect/racket -> racket/base (procedure)
   #:t (number? (sqr 10))

   ;; racket/base -> effect/racket -> racket/base (macro)
   #:t (zero? ((thunk 0)))

   ;; effect/racket (phase 1) -> racket/base
   ;; Like typed/racket, phase n > 0 is normal racket/base.
   #:t (void? ((dynamic-require-from-syntax mod-d 'b)))

   ;; racket/base -> effect/racket (procedure)
   #:x (dynamic-require mod-c #f)
   "cannot be used in a foreign language"

   ;; racket/base -> effect/racket (macro)
   #:x (eval
        '(module d effect/racket
           (require (only-in racket/base time))
           (time (void)))
        (make-base-namespace))
   "cannot be used in a foreign language"

   ;; effect/racket -> racket/base (procedure)
   #:x (f)
   "cannot be used in a foreign language"

   ;; effect/racket -> racket/base (macro)
   #:x (convert-compile-time-error (boo))
   "cannot be used in a foreign language"
   ))
