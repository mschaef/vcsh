
;;;; exports.scm --
;;;;
;;;; Long lists of symbols exported from the image.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export! '(*
             *allow-rich-writes*
             *arg-values*
             *current-load-file*
             *default-tab-stop-interval*
             *default-width*
             *enter-repl-on-runtime-error*
             *error*
             *filename-delimiter*
             *flonum-print-precision*
             *global-readsharp-table*
             *idle-hook*
             *info*
             *last-error*
             *location-mapping*
             *memoize-results?*
             *number-of-characters*
             *package*
             *post-load-hook*
             *pre-load-hook*
             *precision*
             *pretty-print-syntax*
             *print-addresses*
             *print-depth*
             *print-length*
             *print-packages-always*
             *print-readably*
             *print-shared-structure*
             *program-launch-name*
             *read-syntax*
             *readsharp-syntax*
             *repl-abbreviations-enabled*
             *repl-post-hook*
             *repl-pre-print-value-hook*
             *repl-pre-read-hook*
             *show-system-frames*
             *shutdown-hook*
             *silent*
             *time-flonum-print-precision*
             *use-debug-printer*
             +
             -
             ->bag
             ->ieee-754-bits
             ->list
             ->package
             ->packages
             ->string
             ->text
             /
             <
             <=
             =
             >
             >=
             EQUAL?
             UNIMPLEMENTED
             _
             _0
             _1
             _2
             _3
             _4
             _5
             a-list-set!
             a-list->hash
             aand
             abort
             abort-fasl-writes
             abs
             acond
             acos
             add-duration
             add-duration!
             add-hook-function!
             add-readsharp-handler
             aif
             alist
             alist-cons
             alist-copy
             alist-delete
             all-classes
             all-instance-slots
             all-iterate-sequence-types
             all-package-symbols
             all-package-variables
             all-structure-types
             all-symbols
             always
             and
             and*
             angle
             any-not?
             any?
             append
             append!
             append-map
             append-map!
             apply
             apropos
             apropos-any-package
             arg
             argument-values
             asin
             ass
             assert
             assoc
             assq
             assv
             at
             atan
             atom?
             awhen
             awhile
             bad-command-argument-value
             basename-type
             begin
             begin-1
             begin-2
             binary-port?
             bind-if-match
             bit-set?
             bits->exact
             bitwise-and
             bitwise-arithmatic-shift-right
             bitwise-not
             bitwise-or
             bitwise-shift-left
             bitwise-shift-right
             bitwise-xor
             block
             boolean
             boolean?
             break
             break!
             break-lines
             build-symbol
             butlast
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
             call-next-method
             call-with-default-read-error-handling
             call-with-input-file
             call-with-input-port
             call-with-new-print-level
             call-with-output-file
             call-with-output-port
             call-with-output-to-string
             call-with-package
             call-with-port
             call-with-values
             cancel-scheduled-events
             canonicalize-filename
             car
             case
             catch
             catch-all
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
             ceval
             char->integer
             char-alphabetic?
             char-alphanumeric?
             char-ci<=?
             char-ci<?
             char-ci=?
             char-ci>=?
             char-ci>?
             char-downcase
             char-hex?
             char-lower-case?
             char-numeric?
             char-octal?
             char-ready?
             char-upcase
             char-upper-case?
             char-whitespace?
             char<=?
             char<?
             char=?
             char>=?
             char>?
             char?
             character
             character->string
             charset-vector
             check
             circular-list
             class-name
             class-of
             class-superclass
             class-superclasses
             class<=?
             class=?
             classes<=?
             classes=?
             clear-repl-history!
             clone-c-data-port
             clone-instance
             close-input-port
             close-output-port
             close-port
             closure
             closure-bindings
             closure?
             columns->instance
             complex
             complex?
             compose
             cond
             cond-match
             cons
             cons*
             copy-structure
             cos
             count
             current-date
             current-debug-port
             current-error-port
             current-input-port
             current-julian-day
             current-modified-julian-day
             current-output-port
             current-time
             data->instance
             date->julian-day
             date->modified-julian-day
             date->string
             date->time-monotonic
             date->time-tai
             date->time-utc
             date-week-day
             date-week-number
             date-year-day
             dbind
             dbind-if-match
             dbind-matches?
             debug-write
             defalias
             defer-until-idle
             define
             define-command-argument
             define-file-argument-handling
             define-generic-function
             define-iterate-sequence-expander
             define-method
             define-package
             define-proto
             define-structure
             define-text
             defmacro
             defmesg
             delete
             delete!
             delete-file
             delete-package!
             delq
             describe
             direct-instance-slots
             directory
             disassemble
             display
             display-packages
             do
             documentation
             dohash
             doiterate
             dolist
             dotimes
             dotted-list?
             dovec
             drop
             drop-while
             duplicates
             duplicates?
             duration
             dynamic-let
             end-of-file
             end-of-list?
             ensure-package!
             env-lookup
             environment
             environment-variable
             eof-object?
             eq?
             equal?
             eqv?             
             error
             error-escape
             etypecase
             eval
             eval-when
             even?
             every?
             exact->bits
             exact->inexact
             exact?
             exists?
             exit
             exp
             exp10
             export!
             exported-package-symbols
             exported?
             expt
             fasl-load
             fasl-write
             fasl-write-op
             fast-op
             fast-read
             fifth
             file-details
             file-exists?
             file-forms
             file-lines
             filename->list
             filename-append-delimiter
             filename-basename
             filename-extension
             filename-first/rest
             filename-last-path-delimiter-pos
             filename-maybe-add-relative-prefix
             filename-no-extension
             filename-path
             filename-path-delimiter-pos
             filename-prepend-delimiter
             filename-string=?
             filter
             filter-by-glob
             find
             find-internal-file
             find-package
             find-symbol
             find-tail
             first
             fixnum
             flatten
             flatten-instance
             flonum
             floor
             flush-port
             flush-whitespace
             fold
             fold-right
             for-each
             forget-all-memoized-results
             form-source-location
             format
             formatter
             fourth
             free-cell
             fresh-line
             gc
             gc-info
             gc-runtime
             gc-status
             generic-function-signatures
             generic-function?
             gensym
             get-output-string
             get-print-details
             get-property
             get-text
             glob-matcher
             handle-condition
             handle-runtime-error
             handler-bind
             has-slot?
             hash
             hash->a-list
             hash->list
             hash-clear!
             hash-copy
             hash-for-each
             hash-has?
             hash-keys
             hash-keys/t
             hash-push!
             hash-ref
             hash-ref*
             hash-remove!
             hash-set!
             hash-type
             hash?
             home-directory
             identity
             ieee-754-bits->
             if
             ignore-error
             ignore-user-break
             imag-part
             import!
             import-package!
             in
             in-package!
             in-trace-level
             include
             incr!
             indent
             inexact-=
             inexact->display-string
             inexact->exact
             inexact?
             infinite?
             info
             input-port?
             insert-ordered
             inspect
             inspect-analyze-object
             instance
             instance-proto
             instance-understands?
             instance-with-slot?
             instance?
             integer->char
             integer?
             intern!
             intern-keyword!
             internal-files
             invoke-hook
             iota
             is-a?
             is-class?
             is-directory-basename?
             is-directory-filename?
             is-file-basename?
             is-file-filename?
             is-filename-exact?
             iseq
             it
             iterate
             iterate/r
             julian-day->time-monotonic
             julian-day->time-tai
             julian-day->time-utc
             key-list->hash
             keyword?
             lambda
             lambda-list-arity
             last
             last-pair
             leap-year?
             length
             length=0?
             length=1?
             length=2?
             length=3?
             length=4?
             let
             let*
             letrec
             list
             list-2d?
             list->filename
             list->hash
             list->hash-set
             list->set
             list->set/eq
             list->vector
             list-all-packages
             list-combinations
             list-copy
             list-corr
             list-cov
             list-index
             list-let
             list-lr
             list-mean
             list-mean-center
             list-ref
             list-sdev
             list-set!
             list-stat-sums
             list-sum
             list?
             load
             load-file
             load-if-exists
             load-internal
             load-path
             local-package-symbols
             local-package-variables
             log
             log10
             macro
             macro?
             macroexpand
             macroexpand!
             macroexpand-1
             macroexpand-1!
             magnitude
             make-class<
             make-composite-symbol
             make-filename
             make-hash
             make-instance
             make-iterate-sequence-expansion
             make-list
             make-package!
             make-polar
             make-queue
             make-rectangular
             make-string
             make-structure-by-name
             make-tree
             make-vector
             map
             map-pair
             match-pattern-variables
             match?
             matches-glob?
             max
             member
             member-index
             memoize
             memq
             memq-index
             memv
             memv-index
             min
             minimal-alist
             mlambda
             modified-julian-day->time-monotonic
             modified-julian-day->time-tai
             modified-julian-day->time-utc
             modulo
             mvbind
             name
             name->string
             name?
             nan?
             nconc
             negate
             negative?
             newline
             normalize-whitespace
             not
             nth
             nth-cdr
             null-list?
             null?
             number
             number->string
             number?
             odd?
             open-c-data-output
             open-debug-port
             open-input-file
             open-input-string
             open-null-port
             open-output-file
             open-output-string
             or
             or*
             orphaned-structure?
             output-port?
             p-list->a-list
             p-list-fold
             package
             package-copy
             package-name
             package-provided?
             package?
             pad-to-width
             pair-fold
             pair-fold-right
             pair?
             partition
             char-path-quote?
             path-string->list
             peek-char
             perf-report
             platform-case
             platform-linux?
             platform-windows?
             pop!
             population-count
             port
             port-at-end?
             port-bandwidth
             port-io-counts
             port-location
             port-mode
             port-name
             port-translate-mode
             positive?
             primitive?
             print-date
             print-time
             print-unreadably
             printer
             private-package-symbols
             procedure
             procedure-arity
             procedure-lambda-list
             procedure-name
             procedure?
             properties
             provide-package!
             push!
             q-dequeue!
             q-empty?
             q-enqueue!
             q-enqueue-list!
             q-items
             qq-expand
             qsort
             quasiquote
             queue?
             quote
             quotient
             radix-sort
             random
             random-case
             random-list-element
             random-subsequence
             rational?
             rcompose
             read
             read-binary-fixnum
             read-binary-flonum
             read-binary-string
             read-char
             read-char-string
             read-date
             read-error
             read-exact-number
             read-failed
             read-from-string
             read-in-package
             read-line
             read-text-until-character
             read-time
             read-token
             readall
             readsharp-return
             real-part
             real?
             realtime
             realtime->seconds
             rectangular->list
             rectangular-values
             recursive-list-copy
             referred-symbol-grep
             referred-symbols
             remainder
             remote
             remove
             remove-hook-function!
             remove-property!
             rename-package!
             repeat
             repl
             repl-choose
             require-package!
             rest
             return
             reverse
             reverse!
             round
             run
             runtime
             runtime-error
             second
             seconds->realtime
             self
             send
             set!
             set-car!
             set-cdr!
             set-char-syntax!
             set-current-debug-port
             set-current-error-port
             set-current-input-port
             set-current-output-port
             set-diff
             set-diff/eq
             set-environment-variable!
             set-isect
             set-isect/eq
             set-port-translate-mode!
             set-property!
             set-random-seed!
             set-structure-slot-by-name!
             set-symbol-value!
             set-text!
             set-union
             set-union/eq
             shadow-symbol!
             show-progress
             show-runtime-error
             show-type-delta
             show-type-stats
             signal
             simple-find-symbol
             simplify-filename-list
             sin
             skip-until
             sleep
             slot-ref
             slot-set!
             slots-ref
             span
             span!
             split-string
             split-string-once
             split-string-once-from-right
             sqrt
             stable
             stats-list?
             strcmp
             string
             string!=
             string!=-ci
             string->date
             string->list
             string->number
             string->uninterned-symbol
             string-append
             string-begins-with?
             string-case-fold
             string-contains-all-of?
             string-contains-one-of?
             string-copy
             string-downcase
             string-downcase!
             string-drop
             string-drop-right
             string-empty?
             string-first-character
             string-first-substring
             string-fold
             string-leftmost
             string-length
             string-quasiquote
             string-ref
             string-replace
             string-rightmost
             string-search
             string-search-from-right
             string-set!
             string-take
             string-take-right
             string-trim
             string-trim-left
             string-trim-right
             string-upcase
             string-upcase!
             string<
             string<-ci
             string<=
             string<=-ci
             string=
             string=-ci
             string>
             string>-ci
             string>=
             string>=-ci
             string?
             structure
             structure-has-slot?
             structure-slot-by-name
             structure-slots
             structure-type
             structure?
             subr
             substring
             subtract-duration
             subtract-duration!
             sxhash
             symbol
             symbol-bound?
             symbol-name
             symbol-package
             symbol-value
             symbol?
             system
             system-info
             table
             table->instances
             take
             take!
             take-until-dot
             take-up-to
             take-while
             take-while!
             tan
             temporary-file-name
             text->boolean
             the-environment
             third
             throw
             thru-table
             time
             time-difference
             time-difference!
             time-monotonic->date
             time-monotonic->julian-day
             time-monotonic->modified-julian-day
             time-monotonic->time-tai
             time-monotonic->time-tai!
             time-monotonic->time-utc
             time-monotonic->time-utc!
             time-resolution
             time-tai->date
             time-tai->julian-day
             time-tai->modified-julian-day
             time-tai->time-monotonic
             time-tai->time-monotonic!
             time-tai->time-utc
             time-tai->time-utc!
             time-utc->date
             time-utc->julian-day
             time-utc->modified-julian-day
             time-utc->time-monotonic
             time-utc->time-monotonic!
             time-utc->time-tai
             time-utc->time-tai!
             time<=?
             time<?
             time=?
             time>=?
             time>?
             toplevel
             toplevel-repl
             trace
             trace-indent
             trace/no-returns
             traced-procedure
             traced-procedure?
             truncate
             type-of
             typecase
             unbind-symbol!
             uncaught-throw
             unexport!
             unhandled-abort
             unintern!
             unique
             unless
             unquote
             unquote-splicing
             unread-char
             unreadable
             unsafe-list?
             untrace
             untrace-all
             untraced-procedure
             unwind-protect
             use-package!
             user-break
             values
             values-tuple
             vector
             vector->list
             vector-copy
             vector-fill!
             vector-for-each
             vector-index
             vector-ref
             vector-resize
             vector-resize!
             vector-set!
             vector?
             watch
             watch-environment
             watch-locals
             when
             which-file
             while
             wild-glob-pattern?
             with-default-read-error-handling
             with-fasl-file
             with-fasl-stream
             with-gensyms
             with-input-port
             with-output-port
             with-output-to-string
             with-package
             with-port
             with-temporary-file
             within?
             write
             write-binary-fixnum
             write-binary-flonum
             write-binary-string
             write-char
             write-qualified
             write-strings
             zero?
             )
           (find-package "scheme")))

