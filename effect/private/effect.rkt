#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [effect (-> procedure? effect?)]
  [effect? predicate/c]
  [effect-procedure (-> effect? procedure?)])
 define-effect
 handle
 continue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     syntax/transformer)
         racket/control
         racket/match
         racket/stxparam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct effect (procedure)
  #:property prop:procedure
  (λ (eff . args)
    (call/comp*
     (λ (kont)
       (abort/cc effect-prompt-tag kont eff args))
     (λ (kont)
       (abort* (λ () (apply (effect-procedure eff) kont args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `define-effect`

(define-syntax (define-effect stx)
  (syntax-parse stx
    [(_ (?name:id ?param:id ...) ?body:expr ...)
     #:do [(define effect-stx
             #'(effect
                (λ (kont ?param ...)
                  (syntax-parameterize ([continue (make-rename-transformer #'kont)])
                    ?body ...))))]
     #:with ?effect-id (syntax-local-lift-expression effect-stx)
     #'(define-match-expander ?name
         (make-match-transformer #'?effect-id)
         (make-variable-like-transformer #'?effect-id))]))

(begin-for-syntax
  (define ((make-match-transformer effect-id) stx)
    (syntax-parse stx
      [(_ ?p ...) #`(list (== #,effect-id) ?p ...)])))

(define effect-prompt-tag
  (make-continuation-prompt-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `continue`

(define-syntax-parameter continue
  (λ (stx)
    (raise-syntax-error #f "use of continue outside of handle" stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `handle`

(define-syntax (handle stx)
  (syntax-parse stx
    [(_ ?e:expr [?p:expr ?v:expr ...] ...)
     #'(let ()
         (define (user-handler eff args kont)
           (syntax-parameterize ([continue (make-rename-transformer #'kont)])
             (match (cons eff args)
               [?p ?v ...] ...
               [_ (fallback eff args user-handler kont)])))
         (install-handler (λ () ?e) user-handler))]))

(define (install-handler proc user-handler)
  (define (handler kont eff args)
    (user-handler eff args (wrap kont user-handler)))
  (call/prompt proc effect-prompt-tag handler))

(define ((wrap proc user-handler) . args)
  (install-handler (λ () (apply proc args)) user-handler))

(define (fallback eff args user-handler original-kont)
  (call/comp*
   (λ (kont)
     (define kont* (extend kont original-kont))
     (abort/cc effect-prompt-tag kont* eff args))
   (λ (kont)
     (define kont* (extend kont original-kont))
     (abort* (λ () (apply (effect-procedure eff) kont args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(define (abort* proc)
  (abort/cc (default-continuation-prompt-tag) proc))

(define (call/comp* proc default-proc)
  (if (continuation-prompt-available? effect-prompt-tag)
      (call/comp proc effect-prompt-tag)
      (call/comp default-proc (default-continuation-prompt-tag))))

(define ((extend k1 k2) . args)
  (call-in-continuation k1 (λ () (apply k2 args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define err-buffer #f)
  (define str-buffer null)
  (define num-buffer null)

  (define-effect (print-str str)
    (set! err-buffer 'print-str))

  (define-effect (print-num n)
    (set! err-buffer 'print-num))

  (define (handle-str proc)
    (handle (proc)
      [(print-str str)
       (set! str-buffer (cons str str-buffer))
       (continue (void))]))

  (define (handle-num proc)
    (handle (proc)
      [(print-num num)
       (set! num-buffer (cons num num-buffer))
       (continue (void))]))

  (define (handle-both proc)
    (handle (proc)
      [(print-str str)
       (set! str-buffer (cons str str-buffer))
       (continue (void))]
      [(print-num num)
       (set! num-buffer (cons num num-buffer))
       (continue (void))]))

  (define (reset-buffers!)
    (set! err-buffer #f)
    (set! str-buffer null)
    (set! num-buffer null))

  (chk
   ;; no handler
   #:do (print-str "hi")
   err-buffer  'print-str
   #:do (reset-buffers!)

   ;; single handler
   #:do (handle-str (λ ()
                      (print-str "hi")
                      (print-str "there")))
   str-buffer  '("there" "hi")
   #:do (reset-buffers!)

   ;; multiple handlers (same `handle`)
   #:do (handle-both (λ () (print-num 42) (print-str "hi")))
   num-buffer  '(42)
   str-buffer  '("hi")
   #:do (reset-buffers!)

   ;; multiple handlers (long sequence)
   #:do (handle-both
         (λ ()
           (print-num 42)
           (print-str "hi")
           (print-str "there")
           (print-num 43)
           (print-num 44)
           (print-str "done!")))
   num-buffer  '(44 43 42)
   str-buffer  '("done!" "there" "hi")
   #:do (reset-buffers!)

   ;; multiple handlers (different `handle`)
   #:do (handle-str (λ () (handle-num (λ () (print-num 42) (print-str "hi")))))
   num-buffer  '(42)
   str-buffer  '("hi")
   #:do (reset-buffers!)
   ))
