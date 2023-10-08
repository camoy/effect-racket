#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require effect-racket
         effect-racket/private/box
         racket/contract
         racket/list
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lnp

(define (lnp ctc)
  (cond
    [(flat/c? ctc) (flat-lnp ctc)]
    [(=>/c? ctc) (arrow-lnp ctc)]
    [(union-contract? ctc) (union-lnp ctc)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node

(struct node (parent))
(struct stem node ())
(struct branch node (blame children))

(define (stem-error st val blm orig-blm)
  (unless st
    (raise-blame-error orig-blm val "~a" val))
  (branch-error (node-parent st) st val blm orig-blm))

(define (branch-error br st val blm orig-blm)
  (match-define (branch par br-blm chs) br)
  (if (blame-same-polarity? br-blm blm)
      (box-set chs (set-remove (box-get chs) st))
      (box-set chs (seteq)))
  (when (set-empty? (box-get chs))
    (stem-error par val br-blm orig-blm)))

(define (blame-same-polarity? blm1 blm2)
  (eq? (blame-original? blm1)
       (blame-original? blm2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flat/c

(struct flat/c (p)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:late-neg-projection
   (λ (self)
     (define lnp (flat-lnp self))
     (λ (blm)
       (define lnp+blm (lnp blm))
       (λ (val neg)
         (lnp+blm val neg #f))))))

(define (flat-lnp ctc)
  (match-define (flat/c p) ctc)
  (λ (blm)
    (λ (val neg par)
      (define blm* (blame-add-missing-party blm neg))
      (unless (p val)
        (stem-error par val blm* blm*))
      val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =>

(define (=> . ctcs)
  (=>/c (drop-right ctcs 1) (last ctcs)))

(struct =>/c (doms rng)
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection
   (λ (self)
     (define lnp (arrow-lnp self))
     (λ (blm)
       (define lnp+blm (lnp blm))
       (λ (val neg)
         (lnp+blm val neg #f))))))

(define (arrow-lnp ctc)
  (match-define (=>/c doms rng) ctc)
  (define dom-lnps (map lnp doms))
  (define rng-lnp (lnp rng))
  (λ (blm)
    (define dom+blms
      (for/list ([dom-lnp (in-list dom-lnps)])
        (dom-lnp (blame-swap blm))))
    (define rng+blm (rng-lnp blm))
    (λ (val neg par)
      (λ args
        (define args*
          (for/list ([dom+blm (in-list dom+blms)]
                     [arg (in-list args)])
            (dom+blm arg neg par)))
        (define res (apply val args*))
        (rng+blm res neg par)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; union/c

(define (union/c . ctcs)
  (union-contract ctcs))

(struct union-contract (ctcs)
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection
   (λ (self)
     (define lnp (union-lnp self))
     (λ (blm)
       (define lnp+blm (lnp blm))
       (λ (val neg)
         (lnp+blm val neg #f))))))

(define (union-lnp ctc)
  (match-define (union-contract ctcs) ctc)
  (define lnps (map lnp ctcs))
  (λ (blm)
    (define lnp+blms
      (for/list ([lnp (in-list lnps)])
        (lnp blm)))
    (λ (val neg par)
      (define chs (box (seteq)))
      (define br (branch par blm chs))
      (for ([_ ctcs])
        (box-set chs (set-add (box-get chs) (stem br))))
      (for/fold ([val val])
                ([lnp+blm (in-list lnp+blms)]
                 [st (in-list (set->list (box-get chs)))])
        (lnp+blm val neg st)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (with (box-service)
    (chk
     #:do (define/contract (f x)
            (=> (flat/c number?) (flat/c number?))
            (if (zero? x) "" x))

     (f 10)  10
     #:x (f "")
     "contract violation"
     #:x (f 0)
     "broke its own contract"

     #:do (define/contract (g x)
            (=> (union/c (flat/c number?)
                         (flat/c string?))
                (union/c (flat/c number?)
                         (flat/c string?)))
            (cond
              [(and (real? x) (positive? x)) (format "~a" x)]
              [(string? x) (string->number x)]))

     (g 5)  "5"
     (g "5")  5
     #:x (g #f)
     "contract violation"
     #:x (g -5)
     "broke its own contract"

     #:do (define (pos? x)
            (and (number? x) (positive? x)))
     #:do (define/contract (make-h)
            (=>
             (union/c
              (=> (flat/c number?) (flat/c string?))
              (=> (flat/c pos?) (flat/c number?))))
            (λ (x)
              (if (even? x)
                  (format "~a" x)
                  x)))

     #:do (define h1 (make-h))
     (h1 10)  "10"
     #:x (h1 -10)
     "contract violation"
     #:x (h1 13)
     "broke its own contract"

     #:do (define h2 (make-h))
     (h2 13)  13
     #:x (h2 10)
     "broke its own contract"
     )))
