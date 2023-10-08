#lang info

(define scribblings
  '(("scribblings/effect-racket.scrbl" ())))

(define deps
  '("base"))

(define build-deps
  '("effect-racket-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
