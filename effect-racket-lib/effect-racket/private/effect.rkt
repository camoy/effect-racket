#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out token)
         (struct-out effect-value)
         (rename-out [-handler handler]
                     [-contract-handler contract-handler])
         handler-append
         handler?
         main-handler?
         contract-handler?
         append-handler?
         continue
         continue*
         splicing-with
         with
         return
         return?
         effect
         effect-value?
         perform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/bool
                     racket/format
                     racket/function
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/transformer
                     syntax/kerncase)
         racket/contract
         racket/control
         racket/function
         racket/list
         racket/match
         racket/splicing
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

(struct handler ())
(struct main-handler handler (proc))
(struct contract-handler handler (proc))
(struct append-handler handler (handlers))

(define effect-prompt-tag
  (make-continuation-prompt-tag))

(define ABSENT (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `effect`

(define-syntax effect
  (syntax-parser
    [(_ ?name:id (~or (?param:id ...) ?var-arg:id))
     #:with ?predicate (format-id #'?name "~a?" #'?name)
     #:with ?token (syntax-local-lift-expression #`(token '?name))
     #:do [(define arity
             (and (attribute ?param)
                  (length (syntax-e #'(?param ...)))))]
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
    (if arity
        (let ([doms (map (const #'any/c) (range arity))])
          #`(->* #,doms (#:fail failure-result/c) any))
        #'procedure?))

  (define (make-match-transformer token arity)
    (syntax-parser
      [(_ ?p ...)
       #:do [(define pat-arity (length (syntax-e #'(?p ...))))]
       #:fail-unless (implies arity (= pat-arity arity)) (arity-error arity)
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
   (if arity
       (procedure-reduce-keyword-arity performer arity null '(#:fail))
       performer)
   name))

(define (perform eff-val [fail ABSENT])
  (match-define (effect-value token args) eff-val)
  (call/comp*
   token fail #f
   (λ (kont)
     (abort/cc effect-prompt-tag kont eff-val fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `return`

(effect return results)

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

(define handler-param (make-parameter #f))

(define-syntax with
  (syntax-parser
    [(_ (?handler ...) ?body:expr ...)
     #:declare ?handler (expr/c #'handler? #:name "with handler")
     #'(install (handler-append ?handler.c ... default-handler)
                (λ () ?body ...))]))

;; Adapted from `splicing-parameterize` in `racket/splicing`
(define-syntax (splicing-with stx)
  (syntax-case stx ()
    [(_ (handler ...) body ...)
     (with-syntax ([(handler/checked ...)
                    (for/list ([handler-stx (in-list (syntax->list #'(handler ...)))])
                      #`(let ([handler-val #,handler-stx])
                          (unless (handler? handler-val)
                            (raise-argument-error 'splicing-with "handler?" handler-val))
                          handler-val))])
       (if (eq? (syntax-local-context) 'expression)
           #'(with (handler/checked ...)
               body ...)
           (let ([introduce (let ([ctx (syntax-local-make-definition-context)])
                              (λ (stx)
                                (internal-definition-context-add-scopes ctx stx)))])
             (with-syntax ([scopeless-id (datum->syntax #f 'scopeless-id)]
                           [scoped-id (introduce (datum->syntax #f 'scoped-id))]
                           [(scoped-body ...) (map introduce (syntax->list #'(body ...)))]
                           [(free-handler-expr ...)
                            (case (syntax-local-context)
                              [(top-level module) #'((handler-param #f))]
                              [else #'()])])
               #'(begin
                   (splicing-parameterize ([handler-param (list handler/checked ...)])
                     (splicing-with-body
                       scopeless-id scoped-id handler-param scoped-body) ...)
                   free-handler-expr ...)))))]))

(define-syntax (splicing-with-body stx)
  (syntax-case stx ()
    [(_ scopeless-id scoped-id handler-param body)
     (let* ([introducer (make-syntax-delta-introducer #'scoped-id #'scopeless-id)]
            [unintro (λ (stx) (introducer stx 'remove))]
            [expanded-body (local-expand #'body (syntax-local-context)
                                         (kernel-form-identifier-list))])
       (kernel-syntax-case expanded-body #f
         [(begin new-body ...)
          (syntax/loc expanded-body
            (begin
              (splicing-with-body scopeless-id scoped-id handler-param new-body)
              ...))]
         [(define-values ids rhs)
          (quasisyntax/loc expanded-body
            (define-values #,(map (maybe unintro) (syntax->list #'ids))
              (install handler-param (λ () (parameterize ([handler-param #f]) rhs)))))]
         [(define-syntaxes ids rhs)
          (quasisyntax/loc expanded-body
            (define-syntaxes #,(map (maybe unintro) (syntax->list #'ids)) rhs))]
         [(begin-for-syntax . _) expanded-body]
         [(module . _) (unintro expanded-body)]
         [(module* . _) expanded-body]
         [(#%require . _) (unintro expanded-body)]
         [(#%provide . _) expanded-body]
         [(#%declare . _) expanded-body]
         [expr
          (syntax/loc expanded-body
            (install handler-param (λ () (parameterize ([handler-param #f]) expr))))]))]))

(define-for-syntax ((maybe unintro) form)
  (if (syntax-property form 'definition-intended-as-local)
      form
      (unintro form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `handler`

(define-syntax -handler
  (syntax-parser
    [(_ [?p:expr ?v:expr ...] ...)
     #'(let ()
         (define (handler-proc eff-val original-kont kont fail)
           (syntax-parameterize
             ([continue (make-rename-transformer #'kont)]
              [continue* (make-rename-transformer #'original-kont)])
             (begin0
               (match eff-val
                 [?p ?v ...] ...
                 [(return results (... ...)) (apply values results)]
                 [_ (fallback eff-val original-kont self fail)])
               (when (handler-param)
                 (handler-param (cons self (handler-param)))))))
         (define self (main-handler handler-proc))
         self)]))

(define (install-main-handler handler proc)
  (match-define (main-handler handler-proc) handler)
  (define (prompt-handler kont eff-val fail)
    (if (contract-mark kont)
        (fallback eff-val kont handler fail)
        (handler-proc eff-val kont (wrap handler kont) fail)))
  (call/prompt (λ () (call-with-values proc return))
               effect-prompt-tag
               prompt-handler))

(define default-handler (-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `contract-handler`

(define PROPAGATE (gensym))

(define-syntax -contract-handler
  (syntax-parser
    [(_ [?p:expr ?v0:expr ...] ...)
     #'(contract-handler
        (λ (eff-val mark)
           (with-contract-continuation-mark mark
             (match eff-val
               [?p ?v0 ...] ...
               [_ (values #f PROPAGATE)]))))]))

(define (install-contract-handler handler proc)
  (match-define (contract-handler handler-proc) handler)
  (define (prompt-handler kont eff-val fail)
    (define mark (contract-mark kont))
    (if mark
        (match (call-with-values (λ () (handler-proc eff-val mark)) list)
          [(list)
           (raise-user-error 'contract-handler "no values returned")]
          [(list _ ... next-handler)
           #:when (eq? PROPAGATE next-handler)
           (fallback eff-val kont handler fail)]
          [(list val ... next-handler)
           #:when (contract-handler? next-handler)
           (install-contract-handler
            next-handler
            (λ () (apply kont val)))]
          [(list _ ... next-handler)
           (raise-user-error 'contract-handler
                             "~a is not a contract handler"
                             next-handler)])
        (fallback eff-val kont handler fail)))
  (call/prompt (λ ()
                 (begin0
                   (proc)
                   (when (handler-param)
                     (handler-param (cons handler (handler-param))))))
               effect-prompt-tag
               prompt-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `append-handler`

(define (handler-append . handlers)
  (define (f h)
    (if (append-handler? h)
        (append-handler-handlers h)
        (list h)))
  (append-handler (append-map f (reverse handlers))))

(define (install-append-handler handler proc)
  (match-define (append-handler handlers) handler)
  (define f
    (for/fold ([proc proc])
              ([handler (in-list handlers)])
      (λ () (install handler proc))))
  (f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic handler operations

(define (install handler proc)
  (cond
    [(main-handler? handler)
     (install-main-handler handler proc)]
    [(append-handler? handler)
     (install-append-handler handler proc)]
    [(contract-handler? handler)
     (install-contract-handler handler proc)]
    [(parameter? handler)
     (define appended-handlers (apply handler-append (handler)))
     (handler null)
     (install appended-handlers proc)]))

(define (wrap handler kont)
  (cont-wrap (curry install handler) kont))

(define (fallback eff-val original-kont handler fail)
  (match eff-val
    [(return results ...) (apply values results)]
    [_
     (define original-kont* (wrap handler original-kont))
     (call/comp*
      (effect-value-token eff-val) fail original-kont*
      (λ (kont)
        (define kont* (cont-append kont original-kont*))
        (abort/cc effect-prompt-tag kont* eff-val fail)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(define (call/comp* token fail original-kont proc)
  (define fail* (if (procedure? fail) fail (const fail)))
  (cond
    [(continuation-prompt-available? effect-prompt-tag)
     (call/comp (λ (kont) (proc kont)) effect-prompt-tag)]
    [(eq? fail ABSENT) (error (token-name token) "no corresponding handler")]
    [original-kont (call-in-continuation original-kont fail*)]
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
