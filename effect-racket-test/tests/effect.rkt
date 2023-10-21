#lang racket/base

(require chk
         effect-racket
         racket/contract
         syntax/macro-testing)

(define err-buffer #f)
(define str-buffer null)
(define num-buffer null)

(effect print-str (str))
(effect print-num (n))
(effect increment ())

(define handler-str
  (handler
   [(print-str str)
    (set! str-buffer (cons str str-buffer))
    (continue (void))]))

(define handler-twice
  (handler
   [(print-num num)
    (continue (print-num (* 2 num)))]))

(define handler-num
  (handler
   [(print-num num)
    (set! num-buffer (cons num num-buffer))
    (continue (void))]))

(define handler-both
  (handler
   [(print-str str)
    (set! str-buffer (cons str str-buffer))
    (continue (void))]
   [(print-num num)
    (set! num-buffer (cons num num-buffer))
    (continue (void))]))

(define handler-seq
  (handler
   [(print-str str)
    (set! str-buffer (cons str str-buffer))
    (with (handler-num)
      (continue* (void)))]))

(define handler-multi
  (handler
   [(print-num num)
    (continue 1 2)]))

(define (reset-buffers!)
  (set! err-buffer #f)
  (set! str-buffer null)
  (set! num-buffer null))

(effect authorized ())

(define (handler-auth auth?)
  (contract-handler
   [(authorized)
    (values auth? (handler-auth auth?))]))

(effect stuff args)

(chk
 ;; single handler
 #:do (with (handler-str)
        (print-str "hi")
        (print-str "there"))
 str-buffer  '("there" "hi")
 #:do (reset-buffers!)

 ;; propagate properly
 #:do (with (handler-str (handler))
        (print-str "hi")
        (print-str "there"))
 str-buffer  '("there" "hi")
 #:do (reset-buffers!)

 ;; shallow
 #:do (with (handler-seq)
        (print-str "hi")
        (print-num 42))
 str-buffer  '("hi")
 num-buffer  '(42)
 #:do (reset-buffers!)

 ;; multiple handlers (same `handle`)
 #:do (with (handler-both)
        (print-num 42)
        (print-str "hi"))
 num-buffer  '(42)
 str-buffer  '("hi")
 #:do (reset-buffers!)

 ;; multiple handlers (long sequence)
 #:do (with (handler-both)
        (print-num 42)
        (print-str "hi")
        (print-str "there")
        (print-num 43)
        (print-num 44)
        (print-str "done!"))
 num-buffer  '(44 43 42)
 str-buffer  '("done!" "there" "hi")
 #:do (reset-buffers!)

 ;; multiple handlers (different `handle`)
 #:do (with (handler-str)
        (with (handler-num)
          (print-num 42)
          (print-str "hi")))
 num-buffer  '(42)
 str-buffer  '("hi")
 #:do (reset-buffers!)

 ;; multiple handlers (other order)
 #:do (with (handler-num)
        (with (handler-str)
          (print-num 42)
          (print-str "hi")))
 num-buffer  '(42)
 str-buffer  '("hi")
 #:do (reset-buffers!)

 ;; multiple handlers (same `with`)
 #:do (with (handler-str handler-num)
        (print-num 42)
        (print-str "hi"))
 num-buffer  '(42)
 str-buffer  '("hi")
 #:do (reset-buffers!)

 ;; multiple handlers (order)
 #:do (with (handler-num handler-twice)
        (print-num 42))
 #:do (with (handler-twice handler-num)
        (print-num 42))
 num-buffer  '(42 84)
 #:do (reset-buffers!)

 ;; append handlers (same `with`)
 #:do (with ((handler-append handler-num handler-twice))
        (print-num 42))
 #:do (with ((handler-append handler-num handler-twice)
             (handler-append handler-twice handler-twice))
        (print-num 42))
 #:do (with ((handler-append (handler-append handler-num handler-twice)))
        (print-num 42))
 num-buffer  '(84 336 84)
 #:do (reset-buffers!)

 ;; multiple return values
 (with (handler-multi)
   (print-num 42))
 (values 1 2)

 ;; varargs
 #:do (define vararg-handler
        (handler
         [(stuff xs ...)
          (continue (foldl + 0 xs))]))
 (with (vararg-handler)
   (stuff 1 2 3))
 6

 ;; failure
 (with (handler-str)
   (+ 1 (print-num 0 #:fail (λ () 41))))
 42

 ;; failure result
 (with (handler-str)
   (+ 1 (print-num 0 #:fail 41)))
 42

 ;; failure reinstall
 (with (handler-str)
   (print-num 0 #:fail 41)
   (print-str "hi")
   (+ 1 2))
 3

 ;; contract handler
 #:do (define/contract (login x)
        (-> (λ (x) (authorized)) any)
        x)
 #:t (with ((handler-auth #t))
       (login 'stuff))

 ;; contract propagated
 #:do (define propagate
        (contract-handler
         [(authorized)
          (values (authorized) propagate)]))
 #:t (with ((handler-auth #t) propagate)
       (login 'stuff))

 ;; multiple values contract handler
 #:do (define/contract (login* x)
        (-> (λ (x)
              (define-values (x y) (authorized))
              (or x y))
            any)
        x)
 #:do (define handler-auth*
        (contract-handler
         [(authorized)
          (values #t #f handler-auth*)]))
 #:t (with (handler-auth*)
       (login* 'stuff))

 #:do (define/contract (lim x)
        (-> (λ (x) (< (increment) 2)) any)
        x)
 #:do (define (increment-service n)
        (contract-handler
         [(increment) (values n (increment-service (add1 n)))]))

 (with ((increment-service 0))
   (lim 1)
   (lim 1))
 1

 #:x
 (with ((increment-service 0))
   (lim 1)
   (lim 1)
   (lim 1))
 "contract violation"

 ;; fail contract handler
 #:do (define/contract (login-fail x)
        (-> (λ (x) (authorized #:fail (λ () #f))) any)
        x)
 #:t (with ((handler-auth #t))
       (login-fail 'stuff))
 #:x (login-fail 'stuff)
 "given: 'stuff"

 ;; error: fail contract
 #:x (authorized #:fail (λ (x) x))
 "expected: a procedure that accepts 0 non-keyword arguments"

 ;; error: insufficient values
 #:do (define empty-handler-auth
        (contract-handler
         [(authorized) (values)]))
 #:x (with (empty-handler-auth)
       (login* 'stuff))
 "no values returned"

 ;; error: not contract handler
 #:do (define bad-handler-auth
        (contract-handler
         [(authorized) (values 1 2)]))
 #:x (with (bad-handler-auth)
       (login* 'stuff))
 "2 is not a contract handler"

 ;; error: shallow
 #:x (with (handler-seq)
       (print-str "hi")
       (print-str "there"))
 "no corresponding handler"
 #:do (reset-buffers!)

 ;; error: normal handler and contract perform
 #:do (define/contract (thingy x)
        (-> (λ (x) (print-str x)) any)
        x)
 #:x (with (handler-str)
       (thingy 'stuff))
 "print-str: no corresponding handler"

 ;; error: contract handler and normal perform
 #:x (with ((handler-auth #t))
       (authorized))
 "authorized: no corresponding handler"

 ;; error: effect pattern arity
 #:x
 (convert-syntax-error
  (handler
   [(print-num num extra)
    (continue #f)]))
 "expected exactly 1 parameter"
 )
