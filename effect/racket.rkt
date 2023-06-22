#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (rename-out [#%mb #%module-begin]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/require-transform
                     racket/provide-transform
                     racket/syntax
                     syntax/strip-context
                     syntax/stx
                     syntax/parse)
         racket/contract
         syntax/wrap-modbeg
         "private/io.rkt"
         "main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct seal (val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; module begin

(define-syntax #%mb
  (make-wrapping-module-begin	#'with-kernel-service))

(define kernel-service
  io-service)

(define-syntax with-kernel-service
  (syntax-parser
    [(_ ?e)
     #'(with (kernel-service) (write-if-not-void ?e))]))

(define (write-if-not-void x)
  (if (void? x) (void) (write x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  effect/racket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require replacement

(begin-for-syntax
  (define require-definition-syntaxes (make-hash)))

(define-syntax (-require stx)
  (syntax-parse stx
    [(_ ?sub ...)
     #:with ?require
     (local-expand #'(require (seal-import (combine-in ?sub ...)))
                   'top-level
                   (list #'module*))
     #:with ([?lhs ?rhs] ...)
     (for/list ([(lhs rhs) (in-hash require-definition-syntaxes)])
       (list (replace-context stx lhs) rhs))
     (hash-clear! require-definition-syntaxes)
     #`(begin
         ?require
         (define ?lhs (seal-flip ?rhs)) ...)]))

(define-syntax seal-import
  (make-require-transformer
   (syntax-parser
     [(_ ?sub)
      (define-values (raw-imports import-srcs)
        (expand-import #'?sub))
      (define imports
        (for/list ([imp (in-list raw-imports)])
          (define id (import-local-id imp))
          (define redirect (datum->syntax id (gensym)))
          (hash-set! require-definition-syntaxes id redirect)
          (struct-copy import imp [local-id redirect])))
      (values imports import-srcs)])))

(define (seal-flip v)
  (cond
    [(seal? v) (seal-val v)]
    [(flat? v) v]
    [else (seal v)]))

(define (flat? v)
  (or (immutable? v)
      (boolean? v)
      (number? v)
      (char? v)
      (pair? v)
      (void? v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide replacement

(define-syntax -provide
  (syntax-parser
    [(_ ?sub ...)
     #'(provide (wrap-out ?sub) ...)]))

(begin-for-syntax
  (define (export->wrapped-rename e)
    (define local-id (export-local-id e))
    (define wrapped-id (generate-temporary))
    (syntax-local-lift-module-end-declaration
     #`(define #,wrapped-id (seal-flip #,local-id)))
    (define out-sym (export-out-sym e))
    #`(rename-out [#,wrapped-id #,out-sym])))

(define-syntax wrap-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ ?spec)
        #:with (?wrap-spec ...)
        (stx-map export->wrapped-rename (expand-export #'?spec modes))
        (pre-expand-export
         #'(combine-out ?wrap-spec ...)
         modes)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide (base)

(provide
 (all-from-out racket/contract)
 (all-from-out "main.rkt")
 *
 +
 -
 /
 <
 <=
 =
 >
 >=
 ;; abort-current-continuation
 abs
 absolute-path?
 acos
 add1
 ;; alarm-evt
 ;; always-evt
 andmap
 angle
 append
 arithmetic-shift
 arity-at-least-value
 arity-at-least?
 asin
 ;; assert-unreachable
 assf
 assoc
 assq
 assv
 assw
 atan
 banner
 bitwise-and
 bitwise-bit-field
 bitwise-bit-set?
 bitwise-ior
 bitwise-not
 bitwise-xor
 boolean?
 bound-identifier=?
 ;; box
 ;; box-cas!
 ;; box-immutable
 ;; box?
 ;; break-enabled
 ;; break-parameterization?
 ;; break-thread
 build-list
 ;; build-path
 ;; build-path/convention-type
 ;; build-string
 ;; build-vector
 ;; byte-pregexp
 ;; byte-pregexp?
 ;; byte-ready?
 ;; byte-regexp
 ;; byte-regexp?
 ;; byte?
 ;; bytes
 ;; bytes->immutable-bytes
 ;; bytes->list
 ;; bytes->path
 ;; bytes->path-element
 ;; bytes->string/latin-1
 ;; bytes->string/locale
 ;; bytes->string/utf-8
 ;; bytes-append
 ;; bytes-close-converter
 ;; bytes-convert
 ;; bytes-convert-end
 ;; bytes-converter?
 ;; bytes-copy
 ;; bytes-copy!
 ;; bytes-environment-variable-name?
 ;; bytes-fill!
 ;; bytes-length
 ;; bytes-open-converter
 ;; bytes-ref
 ;; bytes-set!
 ;; bytes-utf-8-index
 ;; bytes-utf-8-length
 ;; bytes-utf-8-ref
 ;; bytes<?
 ;; bytes=?
 ;; bytes>?
 ;; bytes?
 caaaar
 caaadr
 caaar
 caadar
 caaddr
 caadr
 caar
 cadaar
 cadadr
 cadar
 caddar
 cadddr
 caddr
 cadr
 ;; call-in-continuation
 ;; call-in-nested-thread
 ;; call-with-break-parameterization
 ;; call-with-composable-continuation
 ;; call-with-continuation-barrier
 ;; call-with-continuation-prompt
 ;; call-with-current-continuation
 ;; call-with-default-reading-parameterization
 ;; call-with-escape-continuation
 ;; call-with-exception-handler
 ;; call-with-immediate-continuation-mark
 ;; call-with-parameterization
 ;; call-with-semaphore
 ;; call-with-semaphore/enable-break
 call-with-values
 ;; call/cc
 ;; call/ec
 car
 cdaaar
 cdaadr
 cdaar
 cdadar
 cdaddr
 cdadr
 cdar
 cddaar
 cddadr
 cddar
 cdddar
 cddddr
 cdddr
 cddr
 cdr
 ceiling
 ;; channel-get
 ;; channel-put
 ;; channel-put-evt
 ;; channel-put-evt?
 ;; channel-try-get
 ;; channel?
 ;; chaperone-box
 ;; chaperone-channel
 ;; chaperone-continuation-mark-key
 ;; chaperone-evt
 ;; chaperone-hash
 ;; chaperone-of?
 ;; chaperone-procedure
 ;; chaperone-procedure*
 ;; chaperone-prompt-tag
 ;; chaperone-struct
 ;; chaperone-struct-type
 ;; chaperone-vector
 ;; chaperone-vector*
 ;; chaperone?
 char->integer
 char-alphabetic?
 char-blank?
 char-ci<=?
 char-ci<?
 char-ci=?
 char-ci>=?
 char-ci>?
 char-downcase
 char-extended-pictographic?
 char-foldcase
 char-general-category
 char-grapheme-break-property
 char-grapheme-step
 char-graphic?
 char-iso-control?
 char-lower-case?
 char-numeric?
 char-punctuation?
 char-ready?
 char-symbolic?
 char-title-case?
 char-titlecase
 char-upcase
 char-upper-case?
 char-utf-8-length
 char-whitespace?
 char<=?
 char<?
 char=?
 char>=?
 char>?
 char?
 check-duplicate-identifier
 checked-procedure-check-and-extract
 ;; choice-evt
 ;; cleanse-path
 ;; close-input-port
 ;; close-output-port
 ;; collect-garbage
 ;; collection-file-path
 ;; collection-path
 ;; compile
 ;; compile-allow-set!-undefined
 ;; compile-context-preservation-enabled
 ;; compile-enforce-module-constants
 ;; compile-syntax
 ;; compile-target-machine?
 ;; compiled-expression-recompile
 ;; compiled-expression?
 ;; compiled-module-expression?
 complete-path?
 complex?
 compose
 compose1
 cons
 ;; continuation-mark-key?
 ;; continuation-mark-set->context
 ;; continuation-mark-set->iterator
 ;; continuation-mark-set->list
 ;; continuation-mark-set->list*
 ;; continuation-mark-set-first
 ;; continuation-mark-set?
 ;; continuation-marks
 ;; continuation-prompt-available?
 ;; continuation-prompt-tag?
 ;; continuation?
 ;; copy-file
 cos
 ;; current-break-parameterization
 ;; current-code-inspector
 ;; current-command-line-arguments
 ;; current-compile
 ;; current-compile-realm
 ;; current-compile-target-machine
 ;; current-compiled-file-roots
 ;; current-continuation-marks
 ;; current-custodian
 ;; current-directory
 ;; current-directory-for-user
 ;; current-drive
 ;; current-environment-variables
 ;; current-error-message-adjuster
 ;; current-error-port
 ;; current-eval
 ;; current-evt-pseudo-random-generator
 ;; current-force-delete-permissions
 ;; current-gc-milliseconds
 ;; current-get-interaction-evt
 ;; current-get-interaction-input-port
 ;; current-inexact-milliseconds
 ;; current-inexact-monotonic-milliseconds
 ;; current-input-port
 ;; current-inspector
 ;; current-library-collection-links
 ;; current-library-collection-paths
 ;; current-load
 ;; current-load-extension
 ;; current-load-relative-directory
 ;; current-load/use-compiled
 ;; current-locale
 ;; current-logger
 ;; current-memory-use
 ;; current-milliseconds
 ;; current-module-declare-name
 ;; current-module-declare-source
 ;; current-module-name-resolver
 ;; current-module-path-for-load
 ;; current-namespace
 ;; current-output-port
 ;; current-parameterization
 ;; current-plumber
 ;; current-preserved-thread-cell-values
 ;; current-print
 ;; current-process-milliseconds
 ;; current-prompt-read
 ;; current-pseudo-random-generator
 ;; current-read-interaction
 ;; current-reader-guard
 ;; current-readtable
 ;; current-seconds
 ;; current-security-guard
 ;; current-subprocess-custodian-mode
 ;; current-subprocess-keep-file-descriptors
 ;; current-thread
 ;; current-thread-group
 ;; current-thread-initial-stack-size
 ;; current-write-relative-directory
 ;; custodian-box-value
 ;; custodian-box?
 ;; custodian-limit-memory
 ;; custodian-managed-list
 ;; custodian-memory-accounting-available?
 ;; custodian-require-memory
 ;; custodian-shut-down?
 ;; custodian-shutdown-all
 ;; custodian?
 ;; custom-print-quotable-accessor
 ;; custom-print-quotable?
 ;; custom-write-accessor
 ;; custom-write?
 ;; date*-nanosecond
 ;; date*-time-zone-name
 ;; date*?
 ;; date-day
 ;; date-dst?
 ;; date-hour
 ;; date-minute
 ;; date-month
 ;; date-second
 ;; date-time-zone-offset
 ;; date-week-day
 ;; date-year
 ;; date-year-day
 ;; date?
 ;; datum->syntax
 ;; datum-intern-literal
 ;; default-continuation-prompt-tag
 ;; delete-directory
 ;; delete-file
 denominator
 ;; directory-exists?
 ;; directory-list
 ;; display
 ;; displayln
 double-flonum?
 ;; dump-memory-stats
 ;; dynamic-require
 ;; dynamic-require-for-syntax
 ;; dynamic-wind
 ;; environment-variables-copy
 ;; environment-variables-names
 ;; environment-variables-ref
 ;; environment-variables-set!
 ;; environment-variables?
 ;; eof
 ;; eof-object?
 ;; ephemeron-value
 ;; ephemeron?
 ;; eprintf
 eq-hash-code
 eq?
 equal-always-hash-code
 equal-always-secondary-hash-code
 equal-always?
 equal-always?/recur
 equal-hash-code
 equal-secondary-hash-code
 equal?
 equal?/recur
 eqv-hash-code
 eqv?
 ;; error
 ;; error-contract->adjusted-string
 ;; error-display-handler
 ;; error-escape-handler
 ;; error-message->adjusted-string
 ;; error-message-adjuster-key
 ;; error-print-context-length
 ;; error-print-source-location
 ;; error-print-width
 ;; error-syntax->string-handler
 ;; error-value->string-handler
 ;; eval
 ;; eval-jit-enabled
 ;; eval-syntax
 even?
 ;; evt?
 exact->inexact
 exact-integer?
 exact-nonnegative-integer?
 exact-positive-integer?
 exact?
 ;; executable-yield-handler
 ;; exit
 ;; exit-handler
 ;; exn-continuation-marks
 ;; exn-message
 ;; exn:break-continuation
 ;; exn:break:hang-up?
 ;; exn:break:terminate?
 ;; exn:break?
 ;; exn:fail:contract:arity?
 ;; exn:fail:contract:continuation?
 ;; exn:fail:contract:divide-by-zero?
 ;; exn:fail:contract:non-fixnum-result?
 ;; exn:fail:contract:variable-id
 ;; exn:fail:contract:variable?
 ;; exn:fail:contract?
 ;; exn:fail:filesystem:errno-errno
 ;; exn:fail:filesystem:errno?
 ;; exn:fail:filesystem:exists?
 ;; exn:fail:filesystem:missing-module-path
 ;; exn:fail:filesystem:missing-module?
 ;; exn:fail:filesystem:version?
 ;; exn:fail:filesystem?
 ;; exn:fail:network:errno-errno
 ;; exn:fail:network:errno?
 ;; exn:fail:network?
 ;; exn:fail:out-of-memory?
 ;; exn:fail:read-srclocs
 ;; exn:fail:read:eof?
 ;; exn:fail:read:non-char?
 ;; exn:fail:read?
 ;; exn:fail:syntax-exprs
 ;; exn:fail:syntax:missing-module-path
 ;; exn:fail:syntax:missing-module?
 ;; exn:fail:syntax:unbound?
 ;; exn:fail:syntax?
 ;; exn:fail:unsupported?
 ;; exn:fail:user?
 ;; exn:fail?
 ;; exn:missing-module-accessor
 ;; exn:missing-module?
 ;; exn:srclocs-accessor
 ;; exn:srclocs?
 ;; exn?
 exp
 ;; expand
 ;; expand-once
 ;; expand-syntax
 ;; expand-syntax-once
 ;; expand-syntax-to-top-form
 ;; expand-to-top-form
 ;; expand-user-path
 ;; explode-path
 expt
 ;; file-exists?
 ;; file-or-directory-identity
 ;; file-or-directory-modify-seconds
 ;; file-or-directory-permissions
 ;; file-or-directory-stat
 ;; file-or-directory-type
 ;; file-position
 ;; file-position*
 ;; file-size
 ;; file-stream-buffer-mode
 ;; file-stream-port?
 ;; file-truncate
 ;; filesystem-change-evt
 ;; filesystem-change-evt-cancel
 ;; filesystem-change-evt?
 ;; filesystem-root-list
 filter
 ;; find-compiled-file-roots
 ;; find-executable-path
 ;; find-library-collection-links
 ;; find-library-collection-paths
 ;; find-system-path
 findf
 fixnum?
 floating-point-bytes->real
 flonum?
 floor
 ;; flush-output
 foldl
 foldr
 ;; for-each
 format
 ;; fprintf
 ;; free-identifier=?
 ;; free-label-identifier=?
 ;; free-template-identifier=?
 ;; free-transformer-identifier=?
 gcd
 ;; generate-temporaries
 ;; gensym
 ;; get-output-bytes
 ;; get-output-string
 ;; getenv
 ;; global-port-print-handler
 ;; guard-evt
 ;; handle-evt
 ;; handle-evt?
 hash
 hash->list
 hash-clear
 ;; hash-clear!
 hash-copy
 hash-count
 hash-empty?
 hash-ephemeron?
 hash-eq?
 hash-equal-always?
 hash-equal?
 hash-eqv?
 hash-for-each
 hash-has-key?
 hash-iterate-first
 hash-iterate-key
 hash-iterate-key+value
 hash-iterate-next
 hash-iterate-pair
 hash-iterate-value
 hash-keys
 hash-keys-subset?
 hash-map
 hash-placeholder?
 hash-ref
 ;; hash-ref!
 hash-ref-key
 hash-remove
 ;; hash-remove!
 hash-set
 hash-set!
 hash-set*
 hash-set*!
 hash-strong?
 hash-update
 ;; hash-update!
 hash-values
 hash-weak?
 hash?
 hashalw
 hasheq
 hasheqv
 ;; identifier-binding
 ;; identifier-binding-portal-syntax
 ;; identifier-binding-symbol
 ;; identifier-distinct-binding
 ;; identifier-label-binding
 ;; identifier-prune-lexical-context
 ;; identifier-prune-to-source-module
 ;; identifier-remove-from-definition-context
 ;; identifier-template-binding
 ;; identifier-transformer-binding
 identifier?
 imag-part
 ;; immutable?
 ;; impersonate-box
 ;; impersonate-channel
 ;; impersonate-continuation-mark-key
 ;; impersonate-hash
 ;; impersonate-procedure
 ;; impersonate-procedure*
 ;; impersonate-prompt-tag
 ;; impersonate-struct
 ;; impersonate-vector
 ;; impersonate-vector*
 ;; impersonator-ephemeron
 ;; impersonator-of?
 ;; impersonator-prop:application-mark
 ;; impersonator-property-accessor-procedure?
 ;; impersonator-property?
 ;; impersonator?
 in-cycle
 in-parallel
 in-sequences
 in-values*-sequence
 in-values-sequence
 inexact->exact
 inexact-real?
 inexact?
 ;; input-port?
 ;; inspector-superior?
 ;; inspector?
 integer->char
 integer->integer-bytes
 integer-bytes->integer
 integer-length
 integer-sqrt
 integer-sqrt/remainder
 integer?
 ;; internal-definition-context-add-scopes
 ;; internal-definition-context-binding-identifiers
 ;; internal-definition-context-introduce
 ;; internal-definition-context-seal
 ;; internal-definition-context-splice-binding-identifier
 ;; internal-definition-context?
 keyword->string
 keyword-apply
 keyword<?
 keyword?
 ;; kill-thread
 lcm
 length
 ;; liberal-define-context?
 ;; link-exists?
 list
 list*
 ;; list->bytes
 list->string
 ;; list->vector
 list-ref
 list-tail
 list?
 ;; load
 ;; load-extension
 ;; load-on-demand-enabled
 ;; load-relative
 ;; load-relative-extension
 ;; load/cd
 ;; load/use-compiled
 ;; local-expand
 ;; local-expand/capture-lifts
 ;; local-transformer-expand
 ;; local-transformer-expand/capture-lifts
 ;; locale-string-encoding
 log
 ;; log-all-levels
 ;; log-level-evt
 ;; log-level?
 ;; log-max-level
 ;; log-message
 ;; log-receiver?
 ;; logger-name
 ;; logger?
 magnitude
 make-arity-at-least
 ;; make-base-empty-namespace
 ;; make-base-namespace
 ;; make-bytes
 ;; make-channel
 ;; make-continuation-mark-key
 ;; make-continuation-prompt-tag
 ;; make-custodian
 ;; make-custodian-box
 ;; make-date
 ;; make-date*
 ;; make-derived-parameter
 ;; make-directory
 ;; make-do-sequence
 ;; make-empty-namespace
 ;; make-environment-variables
 ;; make-ephemeron
 ;; make-ephemeron-hash
 ;; make-ephemeron-hashalw
 ;; make-ephemeron-hasheq
 ;; make-ephemeron-hasheqv
 ;; make-exn
 ;; make-exn:break
 ;; make-exn:break:hang-up
 ;; make-exn:break:terminate
 ;; make-exn:fail
 ;; make-exn:fail:contract
 ;; make-exn:fail:contract:arity
 ;; make-exn:fail:contract:continuation
 ;; make-exn:fail:contract:divide-by-zero
 ;; make-exn:fail:contract:non-fixnum-result
 ;; make-exn:fail:contract:variable
 ;; make-exn:fail:filesystem
 ;; make-exn:fail:filesystem:errno
 ;; make-exn:fail:filesystem:exists
 ;; make-exn:fail:filesystem:missing-module
 ;; make-exn:fail:filesystem:version
 ;; make-exn:fail:network
 ;; make-exn:fail:network:errno
 ;; make-exn:fail:out-of-memory
 ;; make-exn:fail:read
 ;; make-exn:fail:read:eof
 ;; make-exn:fail:read:non-char
 ;; make-exn:fail:syntax
 ;; make-exn:fail:syntax:missing-module
 ;; make-exn:fail:syntax:unbound
 ;; make-exn:fail:unsupported
 ;; make-exn:fail:user
 ;; make-file-or-directory-link
 ;; make-hash
 ;; make-hash-placeholder
 ;; make-hashalw
 ;; make-hashalw-placeholder
 ;; make-hasheq
 ;; make-hasheq-placeholder
 ;; make-hasheqv
 ;; make-hasheqv-placeholder
 ;; make-immutable-hash
 ;; make-immutable-hashalw
 ;; make-immutable-hasheq
 ;; make-immutable-hasheqv
 ;; make-impersonator-property
 ;; make-input-port
 ;; make-inspector
 ;; make-interned-syntax-introducer
 ;; make-keyword-procedure
 ;; make-known-char-range-list
 ;; make-log-receiver
 ;; make-logger
 ;; make-output-port
 ;; make-parameter
 ;; make-phantom-bytes
 ;; make-pipe
 ;; make-placeholder
 ;; make-plumber
 ;; make-polar
 ;; make-portal-syntax
 ;; make-prefab-struct
 ;; make-pseudo-random-generator
 ;; make-reader-graph
 ;; make-readtable
 ;; make-rectangular
 ;; make-rename-transformer
 ;; make-resolved-module-path
 ;; make-security-guard
 ;; make-semaphore
 ;; make-set!-transformer
 ;; make-shared-bytes
 ;; make-sibling-inspector
 ;; make-special-comment
 ;; make-srcloc
 ;; make-string
 ;; make-struct-field-accessor
 ;; make-struct-field-mutator
 ;; make-struct-type
 ;; make-struct-type-property
 ;; make-syntax-delta-introducer
 ;; make-syntax-introducer
 ;; make-thread-cell
 ;; make-thread-group
 ;; make-vector
 ;; make-weak-box
 ;; make-weak-hash
 ;; make-weak-hashalw
 ;; make-weak-hasheq
 ;; make-weak-hasheqv
 ;; make-will-executor
 map
 max
 ;; mcar
 ;; mcdr
 ;; mcons
 member
 memf
 ;; memory-order-acquire
 ;; memory-order-release
 memq
 memv
 memw
 min
 ;; module->exports
 ;; module->imports
 ;; module->indirect-exports
 ;; module->language-info
 ;; module->namespace
 ;; module->realm
 ;; module-cache-clear!
 ;; module-compiled-cross-phase-persistent?
 ;; module-compiled-exports
 ;; module-compiled-imports
 ;; module-compiled-indirect-exports
 ;; module-compiled-language-info
 ;; module-compiled-name
 ;; module-compiled-realm
 ;; module-compiled-submodules
 ;; module-declared?
 ;; module-path-index-join
 ;; module-path-index-resolve
 ;; module-path-index-split
 ;; module-path-index-submodule
 ;; module-path-index?
 ;; module-path?
 ;; module-predefined?
 ;; module-provide-protected?
 modulo
 ;; mpair?
 ;; nack-guard-evt
 ;; namespace-anchor->empty-namespace
 ;; namespace-anchor->namespace
 ;; namespace-anchor?
 ;; namespace-attach-module
 ;; namespace-attach-module-declaration
 ;; namespace-base-phase
 ;; namespace-call-with-registry-lock
 ;; namespace-mapped-symbols
 ;; namespace-module-identifier
 ;; namespace-module-registry
 ;; namespace-require
 ;; namespace-require/constant
 ;; namespace-require/copy
 ;; namespace-require/expansion-time
 ;; namespace-set-variable-value!
 ;; namespace-symbol->identifier
 ;; namespace-syntax-introduce
 ;; namespace-undefine-variable!
 ;; namespace-unprotect-module
 ;; namespace-variable-value
 ;; namespace?
 negative?
 ;; never-evt
 ;; newline
 ;; normal-case-path
 not
 null
 null?
 number->string
 number?
 numerator
 object-name
 odd?
 ;; open-input-bytes
 ;; open-input-string
 ;; open-output-bytes
 ;; open-output-string
 ormap
 ;; output-port?
 pair?
 ;; parameter-procedure=?
 ;; parameter?
 ;; parameterization?
 ;; path->bytes
 ;; path->complete-path
 ;; path->directory-path
 ;; path->string
 ;; path-add-extension
 ;; path-add-suffix
 ;; path-convention-type
 ;; path-element->bytes
 ;; path-element->string
 ;; path-for-some-system?
 ;; path-list-string->path-list
 ;; path-replace-extension
 ;; path-replace-suffix
 ;; path-string?
 ;; path<?
 ;; path?
 ;; peek-byte
 ;; peek-byte-or-special
 ;; peek-bytes
 ;; peek-bytes!
 ;; peek-bytes-avail!
 ;; peek-bytes-avail!*
 ;; peek-bytes-avail!/enable-break
 ;; peek-char
 ;; peek-char-or-special
 ;; peek-string
 ;; peek-string!
 ;; phantom-bytes?
 ;; pipe-content-length
 ;; placeholder-get
 ;; placeholder-set!
 ;; placeholder?
 ;; plumber-add-flush!
 ;; plumber-flush-all
 ;; plumber-flush-handle-remove!
 ;; plumber-flush-handle?
 ;; plumber?
 ;; poll-guard-evt
 ;; port-closed-evt
 ;; port-closed?
 ;; port-commit-peeked
 ;; port-count-lines!
 ;; port-count-lines-enabled
 ;; port-counts-lines?
 ;; port-display-handler
 ;; port-file-identity
 ;; port-file-unlock
 ;; port-next-location
 ;; port-print-handler
 ;; port-progress-evt
 ;; port-provides-progress-evts?
 ;; port-read-handler
 ;; port-try-file-lock?
 ;; port-waiting-peer?
 ;; port-write-handler
 ;; port-writes-atomic?
 ;; port-writes-special?
 ;; port?
 ;; portal-syntax-content
 ;; portal-syntax?
 positive?
 prefab-key->struct-type
 prefab-key?
 prefab-struct-key
 prefab-struct-type-key+field-count
 pregexp
 pregexp?
 primitive-closure?
 primitive-result-arity
 primitive?
 ;; print
 ;; print-as-expression
 ;; print-boolean-long-form
 ;; print-box
 ;; print-graph
 ;; print-hash-table
 ;; print-mpair-curly-braces
 ;; print-pair-curly-braces
 ;; print-reader-abbreviations
 ;; print-struct
 ;; print-syntax-width
 ;; print-unreadable
 ;; print-value-columns
 ;; print-vector-length
 ;; printf
 ;; println
 procedure->method
 procedure-arity
 procedure-arity-includes?
 procedure-arity-mask
 procedure-arity?
 ;; procedure-closure-contents-eq?
 ;; procedure-extract-target
 ;; procedure-impersonator*?
 procedure-keywords
 ;; procedure-realm
 procedure-reduce-arity
 procedure-reduce-arity-mask
 procedure-reduce-keyword-arity
 procedure-reduce-keyword-arity-mask
 procedure-rename
 procedure-result-arity
 ;; procedure-specialize
 ;; procedure-struct-type?
 procedure?
 ;; progress-evt?
 ;; prop:arity-string
 ;; prop:authentic
 ;; prop:checked-procedure
 ;; prop:custom-print-quotable
 ;; prop:custom-write
 prop:equal+hash
 ;; prop:evt
 ;; prop:exn:missing-module
 ;; prop:exn:srclocs
 ;; prop:expansion-contexts
 ;; prop:impersonator-of
 ;; prop:input-port
 ;; prop:liberal-define-context
 prop:object-name
 ;; prop:output-port
 prop:procedure
 ;; prop:rename-transformer
 ;; prop:sealed
 ;; prop:sequence
 ;; prop:set!-transformer
 ;; pseudo-random-generator->vector
 ;; pseudo-random-generator-vector?
 ;; pseudo-random-generator?
 ;; putenv
 quotient
 quotient/remainder
 ;; raise
 ;; raise-argument-error
 ;; raise-argument-error*
 ;; raise-arguments-error
 ;; raise-arguments-error*
 ;; raise-arity-error
 ;; raise-arity-error*
 ;; raise-arity-mask-error
 ;; raise-arity-mask-error*
 ;; raise-mismatch-error
 ;; raise-range-error
 ;; raise-range-error*
 ;; raise-result-arity-error
 ;; raise-result-arity-error*
 ;; raise-result-error
 ;; raise-result-error*
 ;; raise-type-error
 ;; raise-user-error
 ;; random
 ;; random-seed
 rational?
 rationalize
 read
 ;; read-accept-bar-quote
 ;; read-accept-box
 ;; read-accept-compiled
 ;; read-accept-dot
 ;; read-accept-graph
 ;; read-accept-infix-dot
 ;; read-accept-lang
 ;; read-accept-quasiquote
 ;; read-accept-reader
 ;; read-byte
 ;; read-byte-or-special
 ;; read-bytes
 ;; read-bytes!
 ;; read-bytes-avail!
 ;; read-bytes-avail!*
 ;; read-bytes-avail!/enable-break
 ;; read-bytes-line
 ;; read-case-sensitive
 ;; read-cdot
 ;; read-char
 ;; read-char-or-special
 ;; read-curly-brace-as-paren
 ;; read-curly-brace-with-tag
 ;; read-decimal-as-inexact
 ;; read-eval-print-loop
 ;; read-installation-configuration-table
 ;; read-language
 ;; read-line
 ;; read-on-demand-source
 ;; read-single-flonum
 ;; read-square-bracket-as-paren
 ;; read-square-bracket-with-tag
 ;; read-string
 ;; read-string!
 ;; read-syntax
 ;; read-syntax-accept-graph
 ;; read-syntax/recursive
 ;; read/recursive
 ;; readtable-mapping
 ;; readtable?
 real->decimal-string
 real->double-flonum
 real->floating-point-bytes
 real->single-flonum
 real-part
 real?
 ;; regexp
 ;; regexp-match
 ;; regexp-match-exact?
 ;; regexp-match-peek
 ;; regexp-match-peek-immediate
 ;; regexp-match-peek-positions
 ;; regexp-match-peek-positions-immediate
 ;; regexp-match-peek-positions-immediate/end
 ;; regexp-match-peek-positions/end
 ;; regexp-match-positions
 ;; regexp-match-positions/end
 ;; regexp-match/end
 ;; regexp-match?
 ;; regexp-max-lookbehind
 ;; regexp-quote
 ;; regexp-replace
 ;; regexp-replace*
 ;; regexp-replace-quote
 ;; regexp-replaces
 ;; regexp-split
 ;; regexp-try-match
 ;; regexp?
 ;; relative-path?
 remainder
 remove
 remove*
 remq
 remq*
 remv
 remv*
 remw
 remw*
 ;; rename-file-or-directory
 ;; rename-transformer-target
 ;; rename-transformer?
 ;; replace-evt
 ;; reroot-path
 ;; resolve-path
 ;; resolved-module-path-name
 ;; resolved-module-path?
 reverse
 round
 ;; seconds->date
 ;; security-guard?
 ;; semaphore-peek-evt
 ;; semaphore-peek-evt?
 ;; semaphore-post
 ;; semaphore-try-wait?
 ;; semaphore-wait
 ;; semaphore-wait/enable-break
 ;; semaphore?
 sequence->stream
 sequence-generate
 sequence-generate*
 sequence?
 ;; set!-transformer-procedure
 ;; set!-transformer?
 ;; set-box!
 ;; set-box*!
 ;; set-mcar!
 ;; set-mcdr!
 ;; set-phantom-bytes!
 ;; set-port-next-location!
 sha1-bytes
 sha224-bytes
 sha256-bytes
 ;; shared-bytes
 ;; shell-execute
 ;; simplify-path
 sin
 single-flonum-available?
 single-flonum?
 ;; sleep
 ;; special-comment-value
 ;; special-comment?
 ;; split-path
 sqrt
 ;; srcloc->string
 ;; srcloc-column
 ;; srcloc-line
 ;; srcloc-position
 ;; srcloc-source
 ;; srcloc-span
 ;; srcloc?
 ;; stencil-vector
 ;; stencil-vector-length
 ;; stencil-vector-mask
 ;; stencil-vector-mask-width
 ;; stencil-vector-ref
 ;; stencil-vector-set!
 ;; stencil-vector-update
 ;; stencil-vector?
 stop-after
 stop-before
 string
 ;; string->bytes/latin-1
 ;; string->bytes/locale
 ;; string->bytes/utf-8
 ;; string->immutable-string
 string->keyword
 string->list
 string->number
 string->path
 string->path-element
 string->symbol
 string->uninterned-symbol
 string->unreadable-symbol
 string-append
 string-append-immutable
 string-ci<=?
 string-ci<?
 string-ci=?
 string-ci>=?
 string-ci>?
 string-copy
 ;; string-copy!
 string-downcase
 string-environment-variable-name?
 ;; string-fill!
 string-foldcase
 string-grapheme-count
 string-grapheme-span
 string-length
 string-locale-ci<?
 string-locale-ci=?
 string-locale-ci>?
 string-locale-downcase
 string-locale-upcase
 string-locale<?
 string-locale=?
 string-locale>?
 string-normalize-nfc
 string-normalize-nfd
 string-normalize-nfkc
 string-normalize-nfkd
 string-port?
 string-ref
 ;; string-set!
 string-titlecase
 string-upcase
 string-utf-8-length
 string<=?
 string<?
 string=?
 string>=?
 string>?
 string?
 ;; struct->vector
;;  struct-accessor-procedure?
 ;; struct-constructor-procedure?
 ;; struct-info
 ;; struct-mutator-procedure?
 ;; struct-predicate-procedure?
 ;; struct-type-authentic?
 ;; struct-type-info
 ;; struct-type-make-constructor
 ;; struct-type-make-predicate
 ;; struct-type-property-accessor-procedure?
 ;; struct-type-property-predicate-procedure?
 ;; struct-type-property?
 ;; struct-type-sealed?
 ;; struct-type?
 ;; struct:arity-at-least
 ;; struct:date
 ;; struct:date*
 ;; struct:exn
 ;; struct:exn:break
 ;; struct:exn:break:hang-up
 ;; struct:exn:break:terminate
 ;; struct:exn:fail
 ;; struct:exn:fail:contract
 ;; struct:exn:fail:contract:arity
 ;; struct:exn:fail:contract:continuation
 ;; struct:exn:fail:contract:divide-by-zero
 ;; struct:exn:fail:contract:non-fixnum-result
 ;; struct:exn:fail:contract:variable
 ;; struct:exn:fail:filesystem
 ;; struct:exn:fail:filesystem:errno
 ;; struct:exn:fail:filesystem:exists
 ;; struct:exn:fail:filesystem:missing-module
 ;; struct:exn:fail:filesystem:version
 ;; struct:exn:fail:network
 ;; struct:exn:fail:network:errno
 ;; struct:exn:fail:out-of-memory
 ;; struct:exn:fail:read
 ;; struct:exn:fail:read:eof
 ;; struct:exn:fail:read:non-char
 ;; struct:exn:fail:syntax
 ;; struct:exn:fail:syntax:missing-module
 ;; struct:exn:fail:syntax:unbound
 ;; struct:exn:fail:unsupported
 ;; struct:exn:fail:user
 ;; struct:srcloc
 struct?
 sub1
 ;; subbytes
 ;; subprocess
 ;; subprocess-group-enabled
 ;; subprocess-kill
 ;; subprocess-pid
 ;; subprocess-status
 ;; subprocess-wait
 ;; subprocess?
 substring
 symbol->string
 symbol-interned?
 symbol-unreadable?
 symbol<?
 symbol?
 ;; sync
 ;; sync/enable-break
 ;; sync/timeout
 ;; sync/timeout/enable-break
 ;; syntax->datum
 ;; syntax->list
 ;; syntax-arm
 ;; syntax-binding-set
 ;; syntax-binding-set->syntax
 ;; syntax-binding-set?
 ;; syntax-bound-phases
 ;; syntax-bound-symbols
 ;; syntax-column
 ;; syntax-debug-info
 ;; syntax-disarm
 ;; syntax-e
 ;; syntax-line
 ;; syntax-local-apply-transformer
 ;; syntax-local-bind-syntaxes
 ;; syntax-local-certifier
 ;; syntax-local-context
 ;; syntax-local-expand-expression
 ;; syntax-local-get-shadower
 ;; syntax-local-identifier-as-binding
 ;; syntax-local-introduce
 ;; syntax-local-lift-context
 ;; syntax-local-lift-expression
 ;; syntax-local-lift-module
 ;; syntax-local-lift-module-end-declaration
 ;; syntax-local-lift-provide
 ;; syntax-local-lift-require
 ;; syntax-local-lift-values-expression
 ;; syntax-local-make-definition-context
 ;; syntax-local-make-delta-introducer
 ;; syntax-local-module-defined-identifiers
 ;; syntax-local-module-exports
 ;; syntax-local-module-interned-scope-symbols
 ;; syntax-local-module-required-identifiers
 ;; syntax-local-name
 ;; syntax-local-phase-level
 ;; syntax-local-submodules
 ;; syntax-local-transforming-module-provides?
 ;; syntax-local-value
 ;; syntax-local-value/immediate
 ;; syntax-original?
 ;; syntax-position
 ;; syntax-property
 ;; syntax-property-preserved?
 ;; syntax-property-remove
 ;; syntax-property-symbol-keys
 ;; syntax-protect
 ;; syntax-rearm
 ;; syntax-recertify
 ;; syntax-shift-phase-level
 ;; syntax-source
 ;; syntax-source-module
 ;; syntax-span
 ;; syntax-taint
 ;; syntax-tainted?
 ;; syntax-track-origin
 ;; syntax-transforming-module-expression?
 ;; syntax-transforming-with-lifts?
 ;; syntax-transforming?
 ;; syntax?
 ;; system-big-endian?
 ;; system-idle-evt
 ;; system-language+country
 ;; system-library-subpath
 ;; system-path-convention-type
 ;; system-type
 tan
 ;; terminal-port?
 ;; thread
 ;; thread-cell-ref
 ;; thread-cell-set!
 ;; thread-cell-values?
 ;; thread-cell?
 ;; thread-dead-evt
 ;; thread-dead?
 ;; thread-group?
 ;; thread-receive
 ;; thread-receive-evt
 ;; thread-resume
 ;; thread-resume-evt
 ;; thread-rewind-receive
 ;; thread-running?
 ;; thread-send
 ;; thread-suspend
 ;; thread-suspend-evt
 ;; thread-try-receive
 ;; thread-wait
 ;; thread/suspend-to-kill
 ;; thread?
 ;; time-apply
 truncate
 ;; unbox
 ;; unbox*
 ;; uncaught-exception-handler
 ;; unquoted-printing-string
 ;; unquoted-printing-string-value
 ;; unquoted-printing-string?
 ;; use-collection-link-paths
 ;; use-compiled-file-check
 ;; use-compiled-file-paths
 ;; use-user-specific-search-paths
 values
 ;; variable-reference->empty-namespace
 ;; variable-reference->module-base-phase
 ;; variable-reference->module-declaration-inspector
 ;; variable-reference->module-path-index
 ;; variable-reference->module-source
 ;; variable-reference->namespace
 ;; variable-reference->phase
 ;; variable-reference->resolved-module-path
 ;; variable-reference-constant?
 ;; variable-reference-from-unsafe?
 ;; variable-reference?
 ;; vector
 ;; vector*-length
 ;; vector*-ref
 ;; vector*-set!
 ;; vector->immutable-vector
 ;; vector->list
 ;; vector->pseudo-random-generator
 ;; vector->pseudo-random-generator!
 ;; vector->values
 ;; vector-cas!
 ;; vector-copy!
 ;; vector-fill!
 ;; vector-immutable
 ;; vector-length
 ;; vector-ref
 ;; vector-set!
 ;; vector-set-performance-stats!
 ;; vector?
 version
 void
 void?
 ;; weak-box-value
 ;; weak-box?
 ;; will-execute
 ;; will-executor?
 ;; will-register
 ;; will-try-execute
 ;; wrap-evt
 write
 ;; write-byte
 ;; write-bytes
 ;; write-bytes-avail
 ;; write-bytes-avail*
 ;; write-bytes-avail-evt
 ;; write-bytes-avail/enable-break
 ;; write-char
 ;; write-special
 ;; write-special-avail*
 ;; write-special-evt
 ;; write-string
 ;; writeln
 zero?
 #%app
 #%datum
 #%declare
 #%expression
 ;; #%module-begin
 #%plain-app
 #%plain-lambda
 #%plain-module-begin
 ;; #%printing-module-begin
 #%provide
 #%require
 #%stratified-body
 #%top
 #%top-interaction
 #%variable-reference
 ...
 :do-in
 =>
 _
 all-defined-out
 all-from-out
 and
 apply
 arity-at-least
 begin
 begin-for-syntax
 begin0
 ;; call-with-input-file
 ;; call-with-input-file*
 ;; call-with-output-file
 ;; call-with-output-file*
 case
 case-lambda
 combine-in
 combine-out
 cond
 date
 date*
 define
 define-for-syntax
 define-logger
 define-namespace-anchor
 define-sequence-syntax
 define-splicing-for-clause-syntax
 define-struct
 define-struct/derived
 define-syntax
 define-syntax-rule
 define-syntaxes
 define-values
 define-values-for-syntax
 do
 else
 except-in
 except-out
 ;; exn
 ;; exn:break
 ;; exn:break:hang-up
 ;; exn:break:terminate
 ;; exn:fail
 ;; exn:fail:contract
 ;; exn:fail:contract:arity
 ;; exn:fail:contract:continuation
 ;; exn:fail:contract:divide-by-zero
 ;; exn:fail:contract:non-fixnum-result
 ;; exn:fail:contract:variable
 ;; exn:fail:filesystem
 ;; exn:fail:filesystem:errno
 ;; exn:fail:filesystem:exists
 ;; exn:fail:filesystem:missing-module
 ;; exn:fail:filesystem:version
 ;; exn:fail:network
 ;; exn:fail:network:errno
 ;; exn:fail:out-of-memory
 ;; exn:fail:read
 ;; exn:fail:read:eof
 ;; exn:fail:read:non-char
 ;; exn:fail:syntax
 ;; exn:fail:syntax:missing-module
 ;; exn:fail:syntax:unbound
 ;; exn:fail:unsupported
 ;; exn:fail:user
 ;; file
 for
 for*
 for*/and
 for*/first
 for*/fold
 for*/fold/derived
 for*/foldr
 for*/foldr/derived
 for*/hash
 for*/hashalw
 for*/hasheq
 for*/hasheqv
 for*/last
 for*/list
 for*/lists
 for*/or
 for*/product
 for*/sum
 for*/vector
 for-label
 for-meta
 for-space
 for-syntax
 for-template
 for/and
 for/first
 for/fold
 for/fold/derived
 for/foldr
 for/foldr/derived
 for/hash
 for/hashalw
 for/hasheq
 for/hasheqv
 for/last
 for/list
 for/lists
 for/or
 for/product
 for/sum
 ;; for/vector
 ;; gen:custom-write
 gen:equal+hash
 gen:equal-mode+hash
 hash-copy-clear
 hash-map/copy
 if
 ;; in-bytes
 ;; in-bytes-lines
 ;; in-directory
 ;; in-ephemeron-hash
 ;; in-ephemeron-hash-keys
 ;; in-ephemeron-hash-pairs
 ;; in-ephemeron-hash-values
 ;; in-hash
 ;; in-hash-keys
 ;; in-hash-pairs
 ;; in-hash-values
 in-immutable-hash
 in-immutable-hash-keys
 in-immutable-hash-pairs
 in-immutable-hash-values
 in-inclusive-range
 in-indexed
 ;; in-input-port-bytes
 ;; in-input-port-chars
 ;; in-lines
 in-list
 ;; in-mlist
 ;; in-mutable-hash
 ;; in-mutable-hash-keys
 ;; in-mutable-hash-pairs
 ;; in-mutable-hash-values
 in-naturals
 ;; in-port
 in-producer
 in-range
 in-string
 in-value
 ;; in-vector
 in-weak-hash
 in-weak-hash-keys
 in-weak-hash-pairs
 in-weak-hash-values
 lambda
 let
 let*
 let*-values
 let-syntax
 let-syntaxes
 let-values
 ;; let/cc
 let/ec
 letrec
 letrec-syntax
 letrec-syntaxes
 letrec-syntaxes+values
 letrec-values
 lib
 local-require
 ;; log-debug
 ;; log-error
 ;; log-fatal
 ;; log-info
 ;; log-warning
 module
 module*
 module+
 only-in
 only-meta-in
 only-space-in
 ;; open-input-file
 ;; open-input-output-file
 ;; open-output-file
 or
 ;; parameterize
 ;; parameterize*
 ;; parameterize-break
 ;; planet
 prefix-in
 prefix-out
 protect-out
 ;; provide
 (rename-out [-provide provide])
 quasiquote
 ;; quasisyntax
 ;; quasisyntax/loc
 quote
 ;; quote-syntax
 ;; quote-syntax/prune
 ;; raise-syntax-error
 regexp-match*
 regexp-match-peek-positions*
 regexp-match-positions*
 relative-in
 rename-in
 rename-out
 ;; require
 (rename-out [-require require])
 ;; set!
 ;; set!-values
 sort
 ;; srcloc
 ;; struct
 ;; struct-copy
 ;; struct-field-index
 ;; struct-out
 ;; struct/derived
 submod
 ;; syntax
 ;; syntax-binding-set-extend
 ;; syntax-case
 ;; syntax-case*
 ;; syntax-deserialize
 ;; syntax-id-rules
 ;; syntax-rules
 ;; syntax-serialize
 ;; syntax/loc
 ;; time
 unless
 unquote
 unquote-splicing
 ;; unsyntax
 ;; unsyntax-splicing
 when
 ;; with-continuation-mark
 ;; with-handlers
 ;; with-handlers*
 ;; with-input-from-file
 ;; with-output-to-file
 ;; with-syntax
 ;; ~?
 ;; ~@
 λ
 ;; ...
 _
 ;; syntax-id-rules
 ;; syntax-rules
 )
