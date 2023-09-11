#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "../main.rkt"
         "../private/box.rkt"
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pure/c

(define pure/c (->e none/c any/c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   (with (box-service)
     (define b (box 0))
     (box-get b))
   0

   #:do (define/contract (get b)
          pure/c
          (box-get b))
   #:x
   (with (box-service)
     (define b (box 0))
     (get b))
   "none/c allows no values"

   #:do (define/contract (shadowed-get b)
          pure/c
          (with (box-service)
            (box-get b)))

   (with (box-service)
     (define b (box 0))
     (shadowed-get b))
   0
   ))
