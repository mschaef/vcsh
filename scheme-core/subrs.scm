;;;; subrs.scm --
;;;;
;;;; Lisp references to native SUBRs.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(%define %closure #.(host-scheme::%subr-by-name "%closure"))
(%define %closure-code #.(host-scheme::%subr-by-name "%closure-code"))
(%define %closure-env #.(host-scheme::%subr-by-name "%closure-env"))
(%define %control-field #.(host-scheme::%subr-by-name "%control-field"))
(%define %copy-structure #.(host-scheme::%subr-by-name "%copy-structure"))
(%define %debug-flags #.(host-scheme::%subr-by-name "%debug-flags"))
(%define %debug-printer #.(host-scheme::%subr-by-name "%debug-printer"))
(%define %define-global #.(host-scheme::%subr-by-name "%define-global"))
(%define %directory #.(host-scheme::%subr-by-name "%directory"))
(%define %dump-heap-state #.(host-scheme::%subr-by-name "%dump-heap-state"))
(%define %%fasl-load #.(host-scheme::%subr-by-name "%%fasl-load"))
(%define %fast-op #.(host-scheme::%subr-by-name "%fast-op"))
(%define %fast-op-args #.(host-scheme::%subr-by-name "%fast-op-args"))
(%define %fast-op-opcode #.(host-scheme::%subr-by-name "%fast-op-opcode"))
(%define %fast-op-next #.(host-scheme::%subr-by-name "%fast-op-next"))
(%define %file-details #.(host-scheme::%subr-by-name "%file-details"))

(%define %get-current-frames #.(host-scheme::%subr-by-name "%get-current-frames"))
(%define %hash-binding-vector #.(host-scheme::%subr-by-name "%hash-binding-vector"))
(%define %immediate? #.(host-scheme::%subr-by-name "%immediate?"))
(%define %internal-files #.(host-scheme::%subr-by-name "%internal-files"))
(%define %list->values #.(host-scheme::%subr-by-name "%list->values"))
(%define %macro-transformer #.(host-scheme::%subr-by-name "%macro-transformer"))
(%define %macrocons #.(host-scheme::%subr-by-name "%macrocons"))
(%define %make-eof #.(host-scheme::%subr-by-name "%make-eof"))
(%define %memref #.(host-scheme::%subr-by-name "%memref"))
(%define %obaddr #.(host-scheme::%subr-by-name "%obaddr"))
(%define %package-bindings #.(host-scheme::%subr-by-name "%package-bindings"))
(%define %package-use-list #.(host-scheme::%subr-by-name "%package-use-list"))
(%define %packagecons #.(host-scheme::%subr-by-name "%packagecons"))
(%define %panic #.(host-scheme::%subr-by-name "%panic"))
(%define %property-list #.(host-scheme::%subr-by-name "%property-list"))
(%define %request-heap-size #.(host-scheme::%subr-by-name "%request-heap-size"))
(%define %set-closure-code #.(host-scheme::%subr-by-name "%set-closure-code"))
(%define %set-closure-env #.(host-scheme::%subr-by-name "%set-closure-env"))
(%define %set-control-field #.(host-scheme::%subr-by-name "%set-control-field"))
(%define %set-debug-flags #.(host-scheme::%subr-by-name "%set-debug-flags"))
(%define %set-fasl-package-list! #.(host-scheme::%subr-by-name "%set-fasl-package-list!"))
(%define %set-interrupt-mask! #.(host-scheme::%subr-by-name "%set-interrupt-mask!"))
(%define %set-package-name #.(host-scheme::%subr-by-name "%set-package-name"))
(%define %set-package-use-list! #.(host-scheme::%subr-by-name "%set-package-use-list!"))
(%define %set-property-list! #.(host-scheme::%subr-by-name "%set-property-list!"))
(%define %set-stack-limit #.(host-scheme::%subr-by-name "%set-stack-limit"))
(%define %set-trap-handler! #.(host-scheme::%subr-by-name "%set-trap-handler!"))
(%define %heap-cell-count-by-typecode #.(host-scheme::%subr-by-name "%heap-cell-count-by-typecode"))
(%define %slot-ref #.(host-scheme::%subr-by-name "%slot-ref"))
(%define %slot-set! #.(host-scheme::%subr-by-name "%slot-set!"))
(%define %startup-args #.(host-scheme::%subr-by-name "%startup-args"))
(%define %stress-c-heap #.(host-scheme::%subr-by-name "%stress-c-heap"))
(%define %stress-lisp-heap #.(host-scheme::%subr-by-name "%stress-lisp-heap"))
(%define %structure-layout #.(host-scheme::%subr-by-name "%structure-layout"))
(%define %structure-length #.(host-scheme::%subr-by-name "%structure-length"))
(%define %structure-ref #.(host-scheme::%subr-by-name "%structure-ref"))
(%define %structure-set! #.(host-scheme::%subr-by-name "%structure-set!"))
(%define %structure? #.(host-scheme::%subr-by-name "%structure?"))
(%define %structurecons #.(host-scheme::%subr-by-name "%structurecons"))
(%define %subr-name #.(host-scheme::%subr-by-name "%subr-name"))
(%define %subr-table #.(host-scheme::%subr-by-name "%subr-table"))
(%define %subr-type-code #.(host-scheme::%subr-by-name "%subr-type-code"))
(%define %symbol-index #.(host-scheme::%subr-by-name "%symbol-index"))
(%define %symbol-globally-bound? #.(host-scheme::%subr-by-name "%symbol-globally-bound?"))
(%define %sysob #.(host-scheme::%subr-by-name "%sysob"))
(%define %system-info #.(host-scheme::%subr-by-name "system-info"))
(%define %time-apply0 #.(host-scheme::%subr-by-name "%time-apply0"))
(%define %trap-handler #.(host-scheme::%subr-by-name "%trap-handler"))
(%define %typecode #.(host-scheme::%subr-by-name "%typecode"))
(%define %unbound-marker #.(host-scheme::%subr-by-name "%unbound-marker"))
(%define %values #.(host-scheme::%subr-by-name "%values"))
(%define %values->list #.(host-scheme::%subr-by-name "%values->list"))

(%define * #.(host-scheme::%subr-by-name "*"))
(%define + #.(host-scheme::%subr-by-name "+"))
(%define - #.(host-scheme::%subr-by-name "-"))
(%define ->ieee-754-bits #.(host-scheme::%subr-by-name "->ieee-754-bits"))
(%define / #.(host-scheme::%subr-by-name "/"))
(%define < #.(host-scheme::%subr-by-name "<"))
(%define <= #.(host-scheme::%subr-by-name "<="))
(%define = #.(host-scheme::%subr-by-name "="))
(%define > #.(host-scheme::%subr-by-name ">"))
(%define >= #.(host-scheme::%subr-by-name ">="))
(%define acos #.(host-scheme::%subr-by-name "acos"))
(%define add-symbol-to-package #.(host-scheme::%subr-by-name "add-symbol-to-package"))
(%define angle #.(host-scheme::%subr-by-name "angle"))
(%define apply #.(host-scheme::%subr-by-name "apply"))
(%define asin #.(host-scheme::%subr-by-name "asin"))
(%define atan #.(host-scheme::%subr-by-name "atan"))
(%define binary-port? #.(host-scheme::%subr-by-name "binary-port?"))
(%define bitwise-and #.(host-scheme::%subr-by-name "bitwise-and"))
(%define bitwise-arithmatic-shift-right #.(host-scheme::%subr-by-name "bitwise-arithmatic-shift-right"))
(%define bitwise-not #.(host-scheme::%subr-by-name "bitwise-not"))
(%define bitwise-or #.(host-scheme::%subr-by-name "bitwise-or"))
(%define bitwise-shift-left #.(host-scheme::%subr-by-name "bitwise-shift-left"))
(%define bitwise-shift-right #.(host-scheme::%subr-by-name "bitwise-shift-right"))
(%define bitwise-xor #.(host-scheme::%subr-by-name "bitwise-xor"))
(%define boolean? #.(host-scheme::%subr-by-name "boolean?"))
(%define car #.(host-scheme::%subr-by-name "car"))
(%define cdr #.(host-scheme::%subr-by-name "cdr"))
(%define cdr* #.(host-scheme::%subr-by-name "cdr*"))
(%define ceiling #.(host-scheme::%subr-by-name "ceiling"))
(%define char->integer #.(host-scheme::%subr-by-name "char->integer"))
(%define char? #.(host-scheme::%subr-by-name "char?"))
(%define character->string #.(host-scheme::%subr-by-name "character->string"))
(%define clone-c-data-port #.(host-scheme::%subr-by-name "clone-c-data-port"))
(%define close-port #.(host-scheme::%subr-by-name "close-port"))
(%define closure? #.(host-scheme::%subr-by-name "closure?"))
(%define complex? #.(host-scheme::%subr-by-name "complex?"))
(%define cons #.(host-scheme::%subr-by-name "cons"))
(%define cos #.(host-scheme::%subr-by-name "cos"))
(%define debug-write #.(host-scheme::%subr-by-name "debug-write"))
(%define delete-file #.(host-scheme::%subr-by-name "delete-file"))
(%define env-lookup #.(host-scheme::%subr-by-name "env-lookup"))
(%define environment #.(host-scheme::%subr-by-name "environment"))
(%define eof-object? #.(host-scheme::%subr-by-name "eof-object?"))
(%define eq? #.(host-scheme::%subr-by-name "eq?"))
(%define equal? #.(host-scheme::%subr-by-name "equal?"))
(%define eqv? #.(host-scheme::%subr-by-name "eqv?"))
(%define exact->inexact #.(host-scheme::%subr-by-name "exact->inexact"))
(%define exact? #.(host-scheme::%subr-by-name "exact?"))
(%define exp #.(host-scheme::%subr-by-name "exp"))
(%define expt #.(host-scheme::%subr-by-name "expt"))
(%define fast-read #.(host-scheme::%subr-by-name "fast-read"))
(%define floor #.(host-scheme::%subr-by-name "floor"))
(%define flush-port #.(host-scheme::%subr-by-name "flush-port"))
(%define flush-whitespace #.(host-scheme::%subr-by-name "flush-whitespace"))
(%define fresh-line #.(host-scheme::%subr-by-name "fresh-line"))
(%define gc #.(host-scheme::%subr-by-name "gc"))
(%define gc-info #.(host-scheme::%subr-by-name "gc-info"))
(%define gc-runtime #.(host-scheme::%subr-by-name "gc-runtime"))
(%define get-output-string #.(host-scheme::%subr-by-name "get-output-string"))
(%define hash->a-list #.(host-scheme::%subr-by-name "hash->a-list"))
(%define hash->list #.(host-scheme::%subr-by-name "hash->list"))
(%define hash-clear! #.(host-scheme::%subr-by-name "hash-clear!"))
(%define hash-copy #.(host-scheme::%subr-by-name "hash-copy"))
(%define hash-has? #.(host-scheme::%subr-by-name "hash-has?"))
(%define hash-ref #.(host-scheme::%subr-by-name "hash-ref"))
(%define hash-ref* #.(host-scheme::%subr-by-name "hash-ref*"))
(%define hash-remove! #.(host-scheme::%subr-by-name "hash-remove!"))
(%define hash-set! #.(host-scheme::%subr-by-name "hash-set!"))
(%define hash-type #.(host-scheme::%subr-by-name "hash-type"))
(%define hash? #.(host-scheme::%subr-by-name "hash?"))
(%define ieee-754-bits-> #.(host-scheme::%subr-by-name "ieee-754-bits->"))
(%define imag-part #.(host-scheme::%subr-by-name "imag-part"))
(%define inexact->display-string #.(host-scheme::%subr-by-name "inexact->display-string"))
(%define inexact->exact #.(host-scheme::%subr-by-name "inexact->exact"))
(%define inexact? #.(host-scheme::%subr-by-name "inexact?"))
(%define infinite? #.(host-scheme::%subr-by-name "infinite?"))
(%define integer->char #.(host-scheme::%subr-by-name "integer->char"))
(%define integer? #.(host-scheme::%subr-by-name "integer?"))
(%define keyword? #.(host-scheme::%subr-by-name "keyword?"))
(%define length #.(host-scheme::%subr-by-name "length"))
(%define list->hash #.(host-scheme::%subr-by-name "list->hash"))
(%define list->vector #.(host-scheme::%subr-by-name "list->vector"))
(%define log #.(host-scheme::%subr-by-name "log"))
(%define macro? #.(host-scheme::%subr-by-name "macro?"))
(%define magnitude #.(host-scheme::%subr-by-name "magnitude"))
(%define make-fasl-reader #.(host-scheme::%subr-by-name "make-fasl-reader"))
(%define make-hash #.(host-scheme::%subr-by-name "make-hash"))
(%define make-polar #.(host-scheme::%subr-by-name "make-polar"))
(%define make-rectangular #.(host-scheme::%subr-by-name "make-rectangular"))
(%define make-vector #.(host-scheme::%subr-by-name "make-vector"))
(%define modulo #.(host-scheme::%subr-by-name "modulo"))
(%define nan? #.(host-scheme::%subr-by-name "nan?"))
(%define newline #.(host-scheme::%subr-by-name "newline"))
(%define not #.(host-scheme::%subr-by-name "not"))
(%define null? #.(host-scheme::%subr-by-name "null?"))
(%define number->string #.(host-scheme::%subr-by-name "number->string"))
(%define number? #.(host-scheme::%subr-by-name "number?"))
(%define open-debug-port #.(host-scheme::%subr-by-name "open-debug-port"))
(%define open-input-string #.(host-scheme::%subr-by-name "open-input-string"))
(%define open-null-input-port #.(host-scheme::%subr-by-name "open-null-input-port"))
(%define open-null-output-port #.(host-scheme::%subr-by-name "open-null-output-port"))
(%define open-null-port #.(host-scheme::%subr-by-name "open-null-port"))
(%define open-output-string #.(host-scheme::%subr-by-name "open-output-string"))
(%define open-raw-input-file #.(host-scheme::%subr-by-name "open-raw-input-file"))
(%define open-raw-output-file #.(host-scheme::%subr-by-name "open-raw-output-file"))
(%define open-text-input-port #.(host-scheme::%subr-by-name "open-text-input-port"))
(%define open-text-output-port #.(host-scheme::%subr-by-name "open-text-output-port"))
(%define package-name #.(host-scheme::%subr-by-name "package-name"))
(%define package? #.(host-scheme::%subr-by-name "package?"))
(%define pair? #.(host-scheme::%subr-by-name "pair?"))
(%define peek-char #.(host-scheme::%subr-by-name "peek-char"))
(%define port? #.(host-scheme::%subr-by-name "port?"))
(%define port-column #.(host-scheme::%subr-by-name "port-column"))
(%define port-row #.(host-scheme::%subr-by-name "port-row"))
(%define port-mode #.(host-scheme::%subr-by-name "port-mode"))
(%define port-name #.(host-scheme::%subr-by-name "port-name"))
(%define port-translate-mode #.(host-scheme::%subr-by-name "port-translate-mode"))
(%define primitive? #.(host-scheme::%subr-by-name "primitive?"))
(%define procedure? #.(host-scheme::%subr-by-name "procedure?"))
(%define quotient #.(host-scheme::%subr-by-name "quotient"))
(%define random #.(host-scheme::%subr-by-name "random"))
(%define rational? #.(host-scheme::%subr-by-name "rational?"))
(%define read-binary-fixnum-u8 #.(host-scheme::%subr-by-name "read-binary-fixnum-u8"))
(%define read-binary-fixnum-s8 #.(host-scheme::%subr-by-name "read-binary-fixnum-s8"))
(%define read-binary-fixnum-u16 #.(host-scheme::%subr-by-name "read-binary-fixnum-u16"))
(%define read-binary-fixnum-s16 #.(host-scheme::%subr-by-name "read-binary-fixnum-s16"))
(%define read-binary-fixnum-u32 #.(host-scheme::%subr-by-name "read-binary-fixnum-u32"))
(%define read-binary-fixnum-s32 #.(host-scheme::%subr-by-name "read-binary-fixnum-s32"))
(%define read-binary-fixnum-u64 #.(host-scheme::%subr-by-name "read-binary-fixnum-u64"))
(%define read-binary-fixnum-s64 #.(host-scheme::%subr-by-name "read-binary-fixnum-s64"))
(%define read-binary-flonum #.(host-scheme::%subr-by-name "read-binary-flonum"))
(%define read-binary-string #.(host-scheme::%subr-by-name "read-binary-string"))
(%define read-char #.(host-scheme::%subr-by-name "read-char"))
(%define read-line #.(host-scheme::%subr-by-name "read-line"))
(%define real-part #.(host-scheme::%subr-by-name "real-part"))
(%define real? #.(host-scheme::%subr-by-name "real?"))
(%define realtime #.(host-scheme::%subr-by-name "realtime"))
(%define realtime-time-zone-offset #.(host-scheme::%subr-by-name "realtime-time-zone-offset"))
(%define remainder #.(host-scheme::%subr-by-name "remainder"))
(%define rich-write #.(host-scheme::%subr-by-name "rich-write"))
(%define round #.(host-scheme::%subr-by-name "round"))
(%define runtime #.(host-scheme::%subr-by-name "runtime"))
(%define set-car! #.(host-scheme::%subr-by-name "set-car!"))
(%define set-cdr! #.(host-scheme::%subr-by-name "set-cdr!"))
(%define set-environment-variable! #.(host-scheme::%subr-by-name "set-environment-variable!"))
(%define set-port-translate-mode! #.(host-scheme::%subr-by-name "set-port-translate-mode!"))
(%define set-random-seed! #.(host-scheme::%subr-by-name "set-random-seed!"))
(%define set-symbol-package! #.(host-scheme::%subr-by-name "set-symbol-package!"))
(%define %set-symbol-vcell! #.(host-scheme::%subr-by-name "%set-symbol-vcell!"))
(%define sin #.(host-scheme::%subr-by-name "sin"))
(%define sleep #.(host-scheme::%subr-by-name "sleep"))
(%define sqrt #.(host-scheme::%subr-by-name "sqrt"))
(%define strcmp #.(host-scheme::%subr-by-name "strcmp"))
(%define string->number #.(host-scheme::%subr-by-name "string->number"))
(%define string->uninterned-symbol #.(host-scheme::%subr-by-name "string->uninterned-symbol"))
(%define string-append #.(host-scheme::%subr-by-name "string-append"))
(%define string-copy #.(host-scheme::%subr-by-name "string-copy"))
(%define string-downcase #.(host-scheme::%subr-by-name "string-downcase"))
(%define string-downcase! #.(host-scheme::%subr-by-name "string-downcase!"))
(%define string-first-character #.(host-scheme::%subr-by-name "string-first-character"))
(%define string-first-substring #.(host-scheme::%subr-by-name "string-first-substring"))
(%define string-length #.(host-scheme::%subr-by-name "string-length"))
(%define string-ref #.(host-scheme::%subr-by-name "string-ref"))
(%define string-search #.(host-scheme::%subr-by-name "string-search"))
(%define string-search-from-right #.(host-scheme::%subr-by-name "string-search-from-right"))
(%define string-set! #.(host-scheme::%subr-by-name "string-set!"))
(%define string-trim #.(host-scheme::%subr-by-name "string-trim"))
(%define string-trim-left #.(host-scheme::%subr-by-name "string-trim-left"))
(%define string-trim-right #.(host-scheme::%subr-by-name "string-trim-right"))
(%define string-upcase #.(host-scheme::%subr-by-name "string-upcase"))
(%define string-upcase! #.(host-scheme::%subr-by-name "string-upcase!"))
(%define string? #.(host-scheme::%subr-by-name "string?"))
(%define substring #.(host-scheme::%subr-by-name "substring"))
(%define sxhash #.(host-scheme::%subr-by-name "sxhash"))
(%define symbol-name #.(host-scheme::%subr-by-name "symbol-name"))
(%define symbol-package #.(host-scheme::%subr-by-name "symbol-package"))
(%define %symbol-vcell #.(host-scheme::%subr-by-name "%symbol-vcell"))
(%define symbol? #.(host-scheme::%subr-by-name "symbol?"))
(%define system #.(host-scheme::%subr-by-name "system"))
(%define system-info #.(host-scheme::%subr-by-name "system-info")) ;; REVISIT: deprecated
(%define tan #.(host-scheme::%subr-by-name "tan"))
(%define temporary-file-name #.(host-scheme::%subr-by-name "temporary-file-name"))
(%define truncate #.(host-scheme::%subr-by-name "truncate"))
(%define vector #.(host-scheme::%subr-by-name "vector"))
(%define vector->list #.(host-scheme::%subr-by-name "vector->list"))
(%define vector-copy #.(host-scheme::%subr-by-name "vector-copy"))
(%define vector-fill! #.(host-scheme::%subr-by-name "vector-fill!"))
(%define vector-ref #.(host-scheme::%subr-by-name "vector-ref"))
(%define vector-resize #.(host-scheme::%subr-by-name "vector-resize"))
(%define vector-set! #.(host-scheme::%subr-by-name "vector-set!"))
(%define vector? #.(host-scheme::%subr-by-name "vector?"))
(%define write-binary-fixnum-u8 #.(host-scheme::%subr-by-name "write-binary-fixnum-u8"))
(%define write-binary-fixnum-s8 #.(host-scheme::%subr-by-name "write-binary-fixnum-s8"))
(%define write-binary-fixnum-u16 #.(host-scheme::%subr-by-name "write-binary-fixnum-u16"))
(%define write-binary-fixnum-s16 #.(host-scheme::%subr-by-name "write-binary-fixnum-s16"))
(%define write-binary-fixnum-u32 #.(host-scheme::%subr-by-name "write-binary-fixnum-u32"))
(%define write-binary-fixnum-s32 #.(host-scheme::%subr-by-name "write-binary-fixnum-s32"))
(%define write-binary-fixnum-u64 #.(host-scheme::%subr-by-name "write-binary-fixnum-u64"))
(%define write-binary-fixnum-s64 #.(host-scheme::%subr-by-name "write-binary-fixnum-s64"))
(%define write-binary-flonum #.(host-scheme::%subr-by-name "write-binary-flonum"))
(%define write-binary-string #.(host-scheme::%subr-by-name "write-binary-string"))
(%define write-char #.(host-scheme::%subr-by-name "write-char"))
(%define write-strings #.(host-scheme::%subr-by-name "write-strings"))
