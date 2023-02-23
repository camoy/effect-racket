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
  [println (-> string? void?)]
  [print (-> string? void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in base: racket/base)
         "effect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read and write

(define-effect (read)
  (continue (base:read-byte (current-input-port))))

(define-effect (write v)
  (base:write-byte v (current-output-port))
  (continue (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auxiliary

(define (println str)
  (print (string-append str (string #\newline))))

(define (print str)
  (for ([byte (in-bytes (string->bytes/utf-8 str))])
    (write byte)))
