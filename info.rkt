#lang info

;; General

(define collection 'multi)
(define pkg-desc "")
(define version "0.0")
(define pkg-authors '(camoy))

;; Dependencies

(define deps
  '("contract-etc"
    "base"))

(define build-deps
  '("chk-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
