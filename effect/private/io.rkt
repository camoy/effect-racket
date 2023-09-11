#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide read
         write
         print
         display

         newline
         writeln
         println
         displayln

         io-service)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in base: racket/base)
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write service

(effect read ())
(effect write (datum))
(effect print (datum))
(effect display (datum))

(define io-service
  (handler
   [(read) (continue (base:read))]
   [(write datum) (continue (base:write datum))]
   [(print datum) (continue (base:print datum))]
   [(display datum) (continue (base:display datum))]))

(define (newline)
  (display #\newline))

(define (writeln x)
  (write x)
  (newline))

(define (println x)
  (print x)
  (newline))

(define (displayln x)
  (display x)
  (newline))
