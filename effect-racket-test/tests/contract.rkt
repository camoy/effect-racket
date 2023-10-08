#lang racket/base

(require chk
         effect-racket
         racket/contract
         racket/match)

(effect other (num))
(effect generating ())

(define (generating-handler val)
  (contract-handler
   [(generating)
    (values val (generating-handler val))]))

(define generator/c
  (-> (with/c (generating-handler #t))
      void?))

(define yield/c
  (->i ([x any/c])
       #:pre/name ()
       "must be generating"
       (generating)
       [result any/c]))

(chk
 ;; `with/c`
 #:do (define/contract (generator f)
        generator/c
        (f)
        (void))

 #:do (define/contract (yield x)
        yield/c
        x)

 (with ((generating-handler #f))
   (generator void))
 (void)

 (with ((generating-handler #f))
   (generator (λ () (yield 42))))
 (void)

 #:x
 (with ((generating-handler #f))
   (yield 42))
 "must be generating"

 ;; `->e`
 #:do (define/contract (g f)
        (-> (->e generating? boolean?) any)
        (f))

 #:do (define/contract (h f)
        (-> (->e any/c number?) any)
        (f))

 #:do (define/contract (i f)
        (-> (->e other? boolean?) any)
        (f))

 #:do (define/contract (j)
        (->e (λ (x) (generating) (generating))
             (λ (x) (generating)))
        (generating))

 #:do (define/contract (k f)
        (-> (dependent->e other?
                          (λ (e)
                            (match e
                              [(other v) (=/c v)])))
            any)
        (f))

 #:do
 (define (generating-handler* val)
   (handler
    [(generating) (continue val)]))

 #:do
 (define (other-handler f)
   (handler
    [(other v) (continue (f v))]))


 (with ((generating-handler* 17)
        (generating-handler #t))
   (j))
 17

 #:t
 (with ((generating-handler* #t))
   (g (λ () (generating))))

 #:x
 (with ((generating-handler* #t))
   (h (λ () (generating))))
 "promised: number?"

 #:x
 (with ((generating-handler* #t))
   (i (λ () (generating))))
 "expected: other?"

 #:t
 (with ((other-handler values))
   (k (λ () (other 42))))

 #:x
 (with ((other-handler add1))
   (k (λ () (other 42))))
 "promised: (=/c 42)"
 )
