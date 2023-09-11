#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out token)
         (struct-out effect-value)
         (rename-out [-handler handler]
                     [-contract-handler contract-handler])
         handler?
         program-handler?
         contract-handler?
         continue
         continue*
         with
         effect
         effect-value?
         perform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/format
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/transformer)
         racket/bool
         racket/contract
         racket/control
         racket/function
         racket/match
         racket/struct
         racket/stxparam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct token (name))
(struct effect-value (token args)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) (token-name (effect-value-token self)))
      (λ (self) (effect-value-args self))))])

(struct handler (proc)
  #:property prop:procedure 0)
(struct program-handler handler ())
(struct contract-handler handler ())

(define effect-prompt-tag
  (make-continuation-prompt-tag))

(define ABSENT (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `effect`

(define-syntax effect
  (syntax-parser
    [(_ ?name:id (?param:id ...))
     #:with ?predicate (format-id #'?name "~a?" #'?name)
     #:with ?token (syntax-local-lift-expression #`(token '?name))
     #:do [(define arity (length (syntax-e #'(?param ...))))]
     #:with ?performer
     (syntax-local-lift-expression #`(make-performer '?name ?token #,arity))
     #:declare ?performer
     (expr/c (performer-contract arity)
             #:arg? #f
             #:name (~a (syntax-e #'?name)))
     #`(begin
         (define (?predicate x)
           (and (effect-value? x)
                (eq? (effect-value-token x) ?token)))
         (define-match-expander ?name
           (make-match-transformer #'?token #,arity)
           (make-variable-like-transformer #'?performer.c)))]))

(begin-for-syntax
  (define (performer-contract arity)
    (define doms
      (for/list ([_ (in-range arity)])
        #'any/c))
    #`(->* #,doms (#:fail failure-result/c) any))

  (define (make-match-transformer token arity)
    (syntax-parser
      [(_ ?p ...)
       #:do [(define pat-arity (length (syntax-e #'(?p ...))))]
       #:fail-unless (= pat-arity arity) (arity-error arity)
       #`(effect-value (== #,token) (list ?p ...))]))

  (define (arity-error arity)
    (define params (pluralize "parameter" arity))
    (format "expected exactly ~a ~a" arity params))

  (define (pluralize word num)
    (if (= num 1) word (string-append word "s"))))

(define (make-performer name token arity)
  (define (performer #:fail [fail ABSENT] . args)
    (perform (effect-value token args) fail))
  (procedure-rename
   (procedure-reduce-keyword-arity performer arity null '(#:fail))
   name))

(define (perform eff-val [fail ABSENT])
  (match-define (effect-value token args) eff-val)
  (call/comp*
   token fail #f
   (λ (kont)
     (abort/cc effect-prompt-tag kont eff-val fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `continue`

(define-syntax-parameter continue
  (λ (stx)
    (raise-syntax-error #f "cannot use continue outside handle" stx)))

(define-syntax-parameter continue*
  (λ (stx)
    (raise-syntax-error #f "cannot use continue* outside handle" stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `with`

(define-syntax with
  (syntax-parser
    [(_ () ?body:expr ...)
     #'(let () ?body ...)]
    [(_ (?handler ?more ...) ?body:expr ...)
     #:declare ?handler (expr/c #'handler? #:name "with handler")
     #'(let ([handler ?handler.c]
             [thk (λ () (with (?more ...) ?body ...))])
         (install handler thk))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `handler`

(define-syntax -handler
  (syntax-parser
    [(_ [?p:expr ?v:expr ...] ...)
     #'(let ()
         (define (user-handler eff-val original-kont kont fail)
           (syntax-parameterize
             ([continue (make-rename-transformer #'kont)]
              [continue* (make-rename-transformer #'original-kont)])
             (match eff-val
               [?p ?v ...] ...
               [_ (fallback eff-val original-kont self fail)])))
         (define self (program-handler user-handler))
         self)]))

(define (install-handler user-handler proc)
  (define (handler kont eff-val fail)
    (if (contract-mark kont)
        (fallback eff-val kont user-handler fail)
        (user-handler eff-val kont (wrap user-handler kont) fail)))
  (call/prompt proc effect-prompt-tag handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `contract-handler`

(define PROPAGATE (gensym))

(define-syntax -contract-handler
  (syntax-parser
    [(_ [?p:expr ?v0:expr ...] ...)
     #'(let ()
         (define (user-handler eff-val mark)
           (with-contract-continuation-mark mark
             (match eff-val
               [?p ?v0 ...] ...
               [_ (values #f PROPAGATE)])))
         (contract-handler user-handler))]))

(define (install-contract-handler user-handler proc)
  (define (handler kont eff-val fail)
    (define mark (contract-mark kont))
    (if mark
        (match (call-with-values (λ () (user-handler eff-val mark)) list)
          [(list)
           (raise-user-error 'contract-handler "no values returned")]
          [(list _ ... next-handler)
           #:when (eq? PROPAGATE next-handler)
           (fallback eff-val kont user-handler fail)]
          [(list val ... next-handler)
           #:when (contract-handler? next-handler)
           (install-contract-handler
            next-handler
            (λ () (apply kont val)))]
          [(list _ ... next-handler)
           (raise-user-error 'contract-handler
                             "~a is not a contract handler"
                             next-handler)])
        (fallback eff-val kont user-handler fail)))
  (call/prompt proc effect-prompt-tag handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic handler operations

(define (install handler proc)
  (if (contract-handler? handler)
      (install-contract-handler handler proc)
      (install-handler handler proc)))

(define (wrap user-handler kont)
  (cont-wrap (curry install user-handler) kont))

(define (fallback eff-val original-kont user-handler fail)
  (call/comp*
   (effect-value-token eff-val) fail original-kont
   (λ (kont)
     (define kont* (cont-append kont (wrap user-handler original-kont)))
     (abort/cc effect-prompt-tag kont* eff-val fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(define (call/comp* token fail kont proc)
  (define fail* (if (procedure? fail) fail (const fail)))
  (cond
    [(continuation-prompt-available? effect-prompt-tag)
     (call/comp (λ (kont) (proc kont)) effect-prompt-tag)]
    [(eq? fail ABSENT) (error (token-name token) "no corresponding handler")]
    [kont (call-in-continuation kont fail*)]
    [else (fail*)]))

(define (contract-mark kont)
  (continuation-mark-set-first
   (continuation-marks kont)
   contract-continuation-mark-key))

(define append-prompt
  (make-continuation-prompt-tag))

(define (cont-append k1 k2)
  (cont-wrap (curry call-in-continuation k1) k2))

(define (cont-wrap proc k)
  (define (capture+abort)
    (call/comp
     (λ (kont) (abort/cc append-prompt kont))
     append-prompt))
  (call/prompt
   (λ () (proc (λ () (call-in-continuation k capture+abort))))
   append-prompt
   values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           syntax/macro-testing)

  (define err-buffer #f)
  (define str-buffer null)
  (define num-buffer null)

  (effect print-str (str))
  (effect print-num (n))
  (effect increment ())

  (define handler-str
    (-handler
      [(print-str str)
       (set! str-buffer (cons str str-buffer))
       (continue (void))]))

  (define handler-num
    (-handler
      [(print-num num)
       (set! num-buffer (cons num num-buffer))
       (continue (void))]))

  (define handler-both
    (-handler
      [(print-str str)
       (set! str-buffer (cons str str-buffer))
       (continue (void))]
      [(print-num num)
       (set! num-buffer (cons num num-buffer))
       (continue (void))]))

  (define handler-seq
    (-handler
     [(print-str str)
      (set! str-buffer (cons str str-buffer))
      (with (handler-num)
        (continue* (void)))]))

  (define handler-multi
    (-handler
     [(print-num num)
      (continue 1 2)]))

  (define (reset-buffers!)
    (set! err-buffer #f)
    (set! str-buffer null)
    (set! num-buffer null))

  (effect authorized ())

  (define (handler-auth auth?)
    (-contract-handler
     [(authorized)
      (values auth? (handler-auth auth?))]))

  (chk
   ;; single handler
   #:do (with (handler-str)
          (print-str "hi")
          (print-str "there"))
   str-buffer  '("there" "hi")
   #:do (reset-buffers!)

   ;; propagate properly
   #:do (with (handler-str (-handler))
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

   ;; multiple return values
   (with (handler-multi)
     (print-num 42))
   (values 1 2)

   ;; failure
   (with (handler-str)
     (+ 1 (print-num 0 #:fail (λ () 41))))
   42

   ;; failure result
   (with (handler-str)
     (+ 1 (print-num 0 #:fail 41)))
   42

   ;; contract handler
   #:do (define/contract (login x)
          (-> (λ (x) (authorized)) any)
          x)
   #:t (with ((handler-auth #t))
         (login 'stuff))

   ;; contract propagated
   #:do (define propagate
          (-contract-handler
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
          (-contract-handler
           [(authorized)
            (values #t #f handler-auth*)]))
   #:t (with (handler-auth*)
         (login* 'stuff))

   #:do (define/contract (lim x)
          (-> (λ (x) (< (increment) 2)) any)
          x)
   #:do (define (increment-service n)
          (-contract-handler
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
          (-contract-handler
           [(authorized) (values)]))
   #:x (with (empty-handler-auth)
         (login* 'stuff))
   "no values returned"

   ;; error: not contract handler
   #:do (define bad-handler-auth
          (-contract-handler
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
    (-handler
     [(print-num num extra)
      (continue #f)]))
   "expected exactly 1 parameter"
   ))
