#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (module a effect/racket
    (require racket/math)

    (provide f
             sqr-ok?
             sqr)

    (define (f) #f)

    (define sqr-ok?
      (procedure? sqr)))

  (require 'a)

  ;; Check that lump interop works
  (chk
   #:t (procedure? sqr)
   #:! #:t sqr-ok?
   #:! #:t (procedure? f)
   ))
