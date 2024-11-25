#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/contract
		    racket/match
                    effect-racket]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require racket/contract
               racket/match
               effect-racket)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Effect Racket}
@author{Cameron Moy}

@defmodule[effect-racket]

@margin-note{
  This package has not been officially released.
  Backwards compatibility is not guaranteed.
}

This package provides support for effect handlers,
both as a library,
and as a language.
Here is an implementation of
first-class mutable references:

@examples[#:eval evaluator #:no-result
  (struct box (default))

  (effect box-get (b))
  (effect box-set (b v))

  (define (store-service [store (hasheq)])
    (handler
      [(box-get b)
       (define r (hash-ref store b (box-default b)))
       (continue r)]
      [(box-set b v)
       (define store* (hash-set store b v))
       (with ((store-service store*))
         (continue* (void)))]))]

We define a struct named @racket[box]
that will hold a default value,
i.e.,
the value to return
if the box has no mapping in the store.
Two operations,
declared with @racket[effect],
will be interpreted by an effect handler.
The @racket[store-service] function takes
in a store and returns a first-class handler
that interprets box effects in that store.

For @racket[box-get],
we look up the given box in the store.
If the box has a mapping in the store,
then the mapped value is returned.
Otherwise,
the default value is returned.
To return a value,
the @racket[continue] function
is invoked.
This function is bound to a
delimited continuation
from the point where the effect was requested
up to and including the handler itself.
In the effect handler literature,
this is known as a deep handler.

For @racket[box-set],
we construct a new store that maps the
box to its new value.
The continuation is then executed in
a context where operations are handled
by a new handler,
with the updated store.
This time,
we use @racket[continue*] to invoke
the delimited continuation
up to but @emph{not} including the handler itself.
In the effect handler literature,
this is known as a shallow handler.
Using @racket[continue*] allows us
to reinterpret subsequent effects
using a different handler.

Here is how this box implementation is used:

@examples[#:eval evaluator #:label #f
  (with ((store-service))
    (define b (box 0))
    (box-set b (add1 (box-get b)))
    (box-get b))]

@section{Language}
@defmodule[effect/racket #:lang]

A language,
@racketmodname[effect/racket],
provides effect handlers
and ensures that all built-in operations
cooperate with effect handlers too.

This program reinterprets @racket[read]
to take input from a list of strings,
rather than standard in.

@examples[#:eval evaluator #:hidden
  (require effect-racket/private/io)]

@; Rather odd bug requires the language to be on the next line:
@;   https://github.com/racket/scribble/issues/46
@examples[#:eval evaluator #:lang
effect/racket

(define (add-from-inputs)
  (+ (read) (read)))

(define (fixed-input-service ins)
  (handler
    [(read)
     (with ((fixed-input-service (cdr ins)))
       (continue* (car ins)))]))]

The function @racket[fixed-input-service]
takes in a list of values
and feeds that list to @racket[read],
one by one.
Here is how that might be used:

@examples[#:eval evaluator #:label #f
  (with ((fixed-input-service '(1 2)))
    (add-from-inputs))]

Note that an @racketmodname[effect/racket]
module can only require from other
@racketmodname[effect/racket] modules.
There is no interoperability between
@racketmodname[effect/racket]
and other languages.
Higher-order values imported across
the language boundary will be sealed,
i.e,
unusable.

@section{Effects}

@defform[(effect id (param ...))]{
  Declares @racket[id] as an effect
  with the given parameters.
  A number of definitions are created
  by this declaration:
  @itemlist[
  @item{@racket[id] as a procedure that performs the given effect.
  This procedure accepts an optional keyword argument
  @racket[#:fail] that,
  given a @racket[failure-result/c],
  calls that thunk (or yields that value)
  when no handler exists for the effect
  in the current context.}

  @item{@racket[id] as a match pattern
  that matches effect values created by @racket[id].}

  @item{@racket[id?] is a predicate that succeeds on
  effect values created by @racket[id].}]
}

@defproc[(effect-value? [v any/c]) boolean?]{
  Returns if @racket[v] is an effect value.
  An effect value is a first-class value
  that represents a request to perform an effect.
  These are the values that handlers match on.
}

@defform[(return val ...)]{
  Once the body expression evaluates to a sequence of values,
  they are implicitly handed to the built-in @racket[return]
  effect. Here is a definition of the @racket[amb] operator:

  @examples[#:eval evaluator #:no-result
  (effect choice ())
  (effect fail ())

  (define amb-service
    (handler
     [(choice) (append (continue #t) (continue #f))]
     [(fail) '()]
     [(return v) (list v)]))]

  So that the @racket[append] works uniformly, @racket[return]
  is used to inject values from pure expressions into a list.
  It can be used as such:

  @examples[#:eval evaluator #:label #f
  (with (amb-service)
    (define a (choice))
    (define b (choice))
    (if (and a b) (fail) (cons a b)))]

  This effect can also be invoked directly, for early return behavior.

  @examples[#:eval evaluator #:label #f
  (with ()
    (+ 1 (return 10)))]
}

@section{Handlers}

@defform[(handler [pat body ...+] ...)]{
  Yields an ordinary effect handler
  that interprets the given effect-value patterns.
  If no pattern matches the requested effect,
  then the effect is propagated to the next handler.
  This form can only handle effects performed
  in normal code,
  @emph{not} code executed
  while checking a contract.

  Both @racket[continue]
  and @racket[continue*]
  are bound in the bodies
  of each handler arm
  to a deep and shallow delimited continuation,
  respectively.
}

@defform[(contract-handler [pat body ...+] ...)]{
  Similar to @racket[handler],
  except it @emph{only} interprets effects
  performed while checking contracts.
  The @racket[body] expressions don't have
  direct access to the continuation,
  rather each body should return several values.
  Initial values are provided to the continuation,
  while the final returned value is a handler
  to be reinstalled.

@examples[#:eval evaluator #:label #f
(effect increment ())

(define (limited? _)
  (< (increment) 2))

(define/contract (f x)
  (-> limited? any)
  x)

(define (increment-service n)
  (contract-handler
    [(increment) (values n (increment-service (add1 n)))]))

(with ((increment-service 0))
  (f 1)
  (f 1))

(eval:error
  (with ((increment-service 0))
    (f 1)
    (f 1)
    (f 1)))]
}

@defproc[(handler-append [v handler?] ...) handler?]{
  Appends the given handlers.

  @examples[#:eval evaluator #:label #f
    (effect go (x))
    (define twice-service
      (handler
        [(go x) (continue (go (* x 2)))]))
    (define decr-service
      (handler
        [(go x) (continue (sub1 x))]))
    (define twice-then-decr-service
      (handler-append decr-service twice-service))
    (with (twice-then-decr-service)
      (go 10))]
}

@defproc[(handler? [v any/c]) boolean?]{
  Returns if @racket[v] is a handler
  (either an ordinary handler or a contract handler).
}

@defproc[(continue [v any/c] ...) any]{
  Bound to the deep delimited continuation
  in the arms of @racket[with].
}

@defproc[(continue* [v any/c] ...) any]{
  Bound to the shallow delimited continuation
  in the arms of @racket[with].
}

@defform[(with (handler ...) body ...+)]{
  Executes @racket[body]
  with the given handlers
  installed,
  nested in the given order.
}

@defform[(splicing-with (handler ...) body ...+)]{
  Variant of @racket[with] that splices forms
  into the enclosing definition context (similar to @racket[begin]).
}

@section{Contracts}

@defproc[(->e [eff contract?] [ret contract?]) contract?]{
  Returns a contract that protects functions
  by ensuring all effects performed when
  that function is applied satisfy @racket[eff]
  and all values provided to the continuation by a handler
  satisfy @racket[ret].

  @examples[#:eval evaluator #:label #f
    (define pure/c (->e none/c any/c))
    (define/contract (my-map f xs)
      (-> pure/c list? list?)
      (map f xs))
    (my-map (λ (x) (add1 x)) '(1 2 3))
    (eval:error (my-map (λ (x) (write x) x) '(1 2 3)))]
}

@defproc[(dependent->e [eff contract?] [make-ret (-> effect-value? contract?)]) contract?]{
  A dependent variant of @racket[->e]
  where the continuation contract
  relies on the effect value.

  @examples[#:eval evaluator #:label #f
    (effect id (v))
    (define/contract (f)
      (dependent->e id? (match-lambda [(id v) (=/c v)]))
      (id 42))
    (with ((handler [(id v) (continue v)]))
      (f))
    (eval:error
      (with ((handler [(id v) (continue 0)]))
        (f)))]
}

@defproc[(with/c [handler handler?] ...) contract?]{
  Returns a contract that protects functions
  by installing the given contract handler
  whenever that function is applied.

  @examples[#:eval evaluator #:label #f
    (effect id-callable? ())
    (define no-id/c
      (let ()
        (define no-id-handler
          (contract-handler
            [(id-callable?) (values #f no-id-handler)]))
        (with/c no-id-handler)))
    (define/contract (do-it thk)
      (-> no-id/c any)
      (thk))
    (define/contract (id x)
      (->* (any/c) #:pre (id-callable?) any)
      x)
    (do-it (λ () (+ 1 1)))
    (eval:error (do-it (λ () (id 1))))]
}
