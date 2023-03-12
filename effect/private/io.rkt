#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 read
 write
 (contract-out
  #;[read (-> (or/c byte? eof-object?))]
  #;[write (-> byte? void?)]
  [println (-> any/c void?)]
  [print (-> any/c void)]
  [displayln (-> any/c void?)]
  [display (-> any/c void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in base: racket/base)
         racket/format
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read and write

(define-effect (read)
  (continue (base:read-byte (current-input-port))))

(define-effect (write v)
  (base:write-byte v (current-output-port))
  (continue (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers

(define (read-bytes amt)
  (list->bytes
   (for/list ([_ (in-range amt)])
     (read))))

;; TODO: read-string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writers

(define (write-bytes bs)
  (for ([b (in-bytes bs)])
    (write b)))

(define (write-string s)
  (write-bytes (string->bytes/utf-8 s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outputs

(define (newline)
  (write-string "\n"))

(define (print v)
  (write-string (~v v)))

(define (println v)
  (print v)
  (newline))

(define (display v)
  (write-string (~a v)))

(define (displayln v)
  (display v)
  (newline))
