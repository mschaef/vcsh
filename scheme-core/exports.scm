
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
             *1
             *2
             *3
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
             *repl-abbreviations-enabled*
             *repl-post-hook*
             *repl-pre-print-value-hook*
             *repl-pre-read-hook*
             *show-system-frames*
             *shutdown-hook*
             *silent*
             *time-flonum-print-precision*
             *use-debug-printer*
             *warning*
             +
             -
             ->
             ->bag
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
             all-iterate-sequence-types
             all-package-symbols
             all-package-variables
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
             assert
             assoc
             assoc-val
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
             cdr*
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
             close-input-port
             close-output-port
             close-port
             closure
             closure-bindings
             closure?
             compile-form
             complex
             complex?
             compose
             cond
             cond-match
             cons
             cons*
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
             debug-message
             defalias
             defer-until-idle
             define
             define-command-argument
             define-file-argument-handling
             define-generic-function
             define-method
             define-package
             define-proto
             define-repl-abbreviation
             define-text
             defmacro
             define-message
             delete
             delete!
             delete-file
             delete-package!
             delq
             describe
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
             environment-variable/boolean
             environment-variable/number
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
             flonum
             floor
             flush-port
             flush-whitespace
             fold
             fold-right
             for-each
             forget-all-memoized-results
             form-source-location
             forward
             format
             formatter
             fourth
             free-cell
             fresh-line
             full-symbol-name
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
             hash-merge
             hash-merge!
             hash-keys/t
             hash-push!
             hash-ref
             hash-ref*
             hash-remove!
             hash-set!
             hash-set-multiple!
             hash?
             home-directory
             identity
             identity-hash
             identity-hash?
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
             integer->char
             integer?
             intern!
             intern-keyword!
             internal-files
             invoke-hook
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
             list->identity-hash
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
             make-fasl-reader
             make-filename
             make-hash
             make-identity-hash
             make-instance
             make-list
             make-package!
             make-polar
             make-queue
             make-rectangular
             make-string
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
             open-file
             open-input-string
             open-file
             open-null-port
             open-null-input-port
             open-null-output-port
             open-output-string
             open-raw-input-file
             open-raw-output-file
             open-text-input-port 
             open-text-output-port
             or
             or*
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
             port?
             port-at-end?
             port-closed?
             port-column
             port-class-name
             port-location
             port-row
             port-mode
             port-name
             port-open?
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
             random-list-element
             random-subsequence
             rational?
             rcompose
             read
             read-binary-fixnum-u8
             read-binary-fixnum-s8
             read-binary-fixnum-u16
             read-binary-fixnum-s16
             read-binary-fixnum-u32
             read-binary-fixnum-s32
             read-binary-fixnum-u64
             read-binary-fixnum-s64
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
             read-lines
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
             reverse
             reverse!
             round
             run
             runtime
             runtime-check
             runtime-error
             same-length?
             second
             seconds->realtime
             self
             send
             set!
             set-car!
             set-cdr!
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
             set-same?
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
             span
             span!
             split-string
             split-string-once
             split-string-once-from-right
             sqrt
             stats-list?
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
             subr
             substring
             subtract-duration
             subtract-duration!
             sxhash
             sxhash-identity
             symbol
             symbol-bound?
             symbol-name
             symbol-package
             symbol-value
             symbol?
             system
             system-info
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
             trace-message
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
             warning
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
             write-binary-fixnum-u8
             write-binary-fixnum-s8
             write-binary-fixnum-u16
             write-binary-fixnum-s16
             write-binary-fixnum-u32
             write-binary-fixnum-s32
             write-binary-fixnum-u64
             write-binary-fixnum-s64
             write-binary-flonum
             write-binary-string
             write-char
             write-qualified
             write-strings
             write-to-string
             zero?)
           (find-package "scheme")))

