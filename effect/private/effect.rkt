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
         effect-value->list
         perform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/transformer)
         racket/contract
         racket/control
         racket/match
         racket/stxparam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct token (name))
(struct effect-value (token args))

(struct handler (proc))
(struct program-handler handler ())
(struct contract-handler handler ())

(define effect-prompt-tag
  (make-continuation-prompt-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `effect`

(define-syntax effect
  (syntax-parser
    [(_ ?name:id (?param:id ...))
     #:with ?predicate (format-id #'?name "~a?" #'?name)
     #:with ?token (syntax-local-lift-expression #`(token '?name))
     #:with ?arity #`#,(length (syntax-e #'(?param ...)))
     #:with ?performer (syntax-local-lift-expression #'(make-performer ?token ?arity))
     #'(begin
         (define (?predicate x)
           (and (effect-value? x)
                (eq? (effect-value-token x) ?token)))
         (define-match-expander ?name
           (make-match-transformer #'?token ?arity)
           (make-variable-like-transformer #'?performer)))]))

(begin-for-syntax
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

(define (make-performer token arity)
  (procedure-reduce-arity
   (λ args (perform (effect-value token args)))
   arity))

(define (perform eff-val)
  (match-define (effect-value token args) eff-val)
  (call/comp*
   token
   (λ (kont)
     (define kont* (make-continuation* kont))
     (abort/cc effect-prompt-tag kont* eff-val))))

(define effect-value->list effect-value-args)

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
     #'(let ([h ?handler.c]
             [thk (λ () (with (?more ...) ?body ...))])
         (if (contract-handler? h)
             (install-contract-handler thk (handler-proc h))
             (install-handler thk (handler-proc h))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `handle`

(define-syntax -handler
  (syntax-parser
    [(_ [?p:expr ?v:expr ...] ...)
     #'(let ()
         (define (user-handler eff-val original-kont kont)
           (syntax-parameterize
               ([continue (make-rename-transformer #'kont)]
                [continue* (make-rename-transformer #'original-kont)])
             (match eff-val
               [?p ?v ...] ...
               [_ (fallback eff-val user-handler original-kont)])))
         (program-handler user-handler))]))

(define (install-handler proc user-handler)
  (define (handler kont eff-val)
    (if (continuation*-mark? kont)
        (fallback eff-val user-handler kont)
        (user-handler eff-val kont (wrap user-handler kont))))
  (call/prompt proc effect-prompt-tag handler))

(define (wrap user-handler kont)
  (match-define (continuation* proc mark?) kont)
  (define (kont* . args)
    (install-handler (λ () (apply proc args)) user-handler))
  (continuation* kont* mark?))

(define (fallback eff-val user-handler original-kont)
  (call/comp*
   (effect-value-token eff-val)
   (λ (kont)
     (define kont* (extend kont original-kont))
     (abort/cc effect-prompt-tag kont* eff-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `handle-contract`

(define propagate (gensym))

(define-syntax -contract-handler
  (syntax-parser
    [(_ [?p:expr ?v0:expr ...] ...)
     #'(let ()
         (define (user-handler eff-val)
           (with-continuation-mark contract-continuation-mark-key #t
             (match eff-val
               [?p ?v0 ...] ...
               [_ (values #f propagate)])))
         (contract-handler user-handler))]))

(define (install-contract-handler proc user-handler)
  (define (handler kont eff-val)
    (if (continuation*-mark? kont)
        (match (call-with-values (λ () (user-handler eff-val)) list)
          [(list)
           (raise-user-error 'contract-handler "no values returned")]
          [(list _ ... next-handler)
           #:when (eq? propagate next-handler)
           (fallback eff-val user-handler kont)]
          [(list val ... next-handler)
           #:when (contract-handler? next-handler)
           (install-handler
            (λ () (apply kont val))
            (handler-proc next-handler))]
          [(list _ ... next-handler)
           (raise-user-error 'contract-handler
                             "~a is not a contract handler"
                             next-handler)])
        (fallback eff-val user-handler kont)))
  (call/prompt proc effect-prompt-tag handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extended continuation

(struct continuation* (proc mark?)
  #:property prop:procedure 0)

(define (make-continuation* k)
  (continuation* k (continuation-contract-mark-present? k)))

(define (extend k1 k2)
  (match-define (continuation* proc present?) k2)
  (continuation*
   (λ args (call-in-continuation k1 (λ () (apply proc args))))
   (or (continuation-contract-mark-present? k1) present?)))

(define (continuation-contract-mark-present? kont)
  (continuation-mark-set-first
   (continuation-marks kont)
   contract-continuation-mark-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(define (call/comp* token proc)
  (if (continuation-prompt-available? effect-prompt-tag)
      (call/comp (λ (kont) (proc kont)) effect-prompt-tag)
      (error (token-name token) "no corresponding handler")))

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
