#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/contract
                    effect]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require racket/contract
               effect)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Effect Racket}
@author{Cameron Moy}

@defmodule[effect]

@margin-note{
  This package has not been officially released.
  Backwards compatibility is not guaranteed.
}

TODO

@examples[#:eval evaluator #:no-result
  (effect increment (k))

  (define increment-service
    (handler
      [(increment k)
      (continue (add1 k))]))]

@examples[#:eval evaluator #:label #f
  (with (increment-service)
    (increment 42))]

@section{Effects}

@defform[(effect id (param ...))]{
  defines three things: perform effect function, match pattern syntax, effect predicate
}

@defproc[(effect-value? [v any/c]) boolean?]{
  Returns if @racket[v] is an effect value.
}

@defproc[(effect-value->list [v effect-value?]) list?]{
  TODO
}

@section{Handlers}

@defform[(handler [pat body ...+] ...)]{
  TODO
}

@defform[(contract-handler [pat body ...+] ...)]{
  TODO
}

@defproc[(handler? [v any/c]) boolean?]{
  Returns if @racket[v] is a handler.
}

@defproc[(continue [v any/c] ...) any]{
  TODO: deep
}

@defproc[(continue* [v any/c] ...) any]{
  TODO: shallow
}

@defform[(with (handler ...) body ...+)]{
  TODO
}

@section{Contracts}

@defproc[(contract-handler/c [handler handler?]) contract?]{
  TODO
}

@defproc[(->e [eff contract?] [ret contract?]) contract?]{
  TODO
}
