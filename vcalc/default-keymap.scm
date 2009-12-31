;;;; default-keymap.scm
;;;; Mike Schaeffer
;;
;; Code to define the default keymaps.


(define (init-busy-keymap)
  (set! *busy-keymap* (make-keymap :name "Busy Keybindings"))
  (keymap-bind-sequence! *busy-keymap* '((:control #\c)) 'vcalc-commands::interactive-break))

(define (init-global-keymap)   
  (set! *global-keymap* (make-keymap :name "Global Keybindings"))

  (keymap-describe-sequence! *global-keymap* '(#\p) "Program>>")
  (keymap-bind-sequence! *global-keymap* '(#\p #\l) 'vcalc-commands::command-library)
  (keymap-bind-sequence! *global-keymap* '(#\p #\[) 'vcalc-commands::begin-macro)
  (keymap-bind-sequence! *global-keymap* '(#\p #\]) 'vcalc-commands::end-macro)
  (keymap-bind-sequence! *global-keymap* '(#\p #\a) 'vcalc-commands::apply-to-stack)
  (keymap-bind-sequence! *global-keymap* '(#\p #\A) 'vcalc-commands::apply-to-stack-repeatedly)

  (keymap-describe-sequence! *global-keymap* '(#\l) "List>>")
  (keymap-bind-sequence! *global-keymap* '(#\l #\() 'vcalc-commands::begin-list)
  (keymap-bind-sequence! *global-keymap* '(#\l #\)) 'vcalc-commands::end-list)
  (keymap-bind-sequence! *global-keymap* '(#\l #\>) 'vcalc-commands::list->)
  (keymap-bind-sequence! *global-keymap* '(#\l #\<) 'vcalc-commands::->list)
  (keymap-bind-sequence! *global-keymap* '(#\l #\f) 'vcalc-commands::first)
  (keymap-bind-sequence! *global-keymap* '(#\l #\F) 'vcalc-commands::rest)
  (keymap-bind-sequence! *global-keymap* '(#\l #\l) 'vcalc-commands::last)
  (keymap-bind-sequence! *global-keymap* '(#\l #\L) 'vcalc-commands::butlast)
  (keymap-bind-sequence! *global-keymap* '(#\l #\u) 'vcalc-commands::union)
  (keymap-bind-sequence! *global-keymap* '(#\l #\d) 'vcalc-commands::diff)
  (keymap-bind-sequence! *global-keymap* '(#\l #\i) 'vcalc-commands::intersect)

  (keymap-bind-sequence! *global-keymap* '(#\i) 'vcalc-commands::*i)
  (keymap-bind-sequence! *global-keymap* '(#\I) 'vcalc-commands::*i/+)

  (keymap-describe-sequence! *global-keymap* '(#\c) "Complex Numbers>>")
  (keymap-bind-sequence! *global-keymap* '(#\c (:control #\r)) 'vcalc-commands::->rectangular)
  (keymap-bind-sequence! *global-keymap* '(#\c (:control #\p)) 'vcalc-commands::->polar)
  (keymap-bind-sequence! *global-keymap* '(#\c #\m) 'vcalc-commands::magnitude)
  (keymap-bind-sequence! *global-keymap* '(#\c #\a) 'vcalc-commands::angle)
  (keymap-bind-sequence! *global-keymap* '(#\c #\r) 'vcalc-commands::real-part)
  (keymap-bind-sequence! *global-keymap* '(#\c #\i) 'vcalc-commands::imag-part)
  (keymap-bind-sequence! *global-keymap* '(#\c #\R) 'vcalc-commands::make-rectangular)
  (keymap-bind-sequence! *global-keymap* '(#\c #\P) 'vcalc-commands::make-polar)

  (keymap-bind-sequence! *global-keymap* '((:control #\e)) 'vcalc-commands::edit-object)
  (keymap-bind-sequence! *global-keymap* '((:control #\E)) 'vcalc-commands::enter-object)

  (keymap-describe-sequence! *global-keymap* '(#\M) "Mode >>")

  (keymap-describe-sequence! *global-keymap* '(#\M #\i) "Interest Accrual >>")
  (keymap-bind-sequence! *global-keymap* '(#\M #\i #\e) 'vcalc-commands::set-ending)
  (keymap-bind-sequence! *global-keymap* '(#\M #\i #\b) 'vcalc-commands::set-beginning)

  (keymap-describe-sequence! *global-keymap* '(#\M #\b) "Default Base >>")
  ; (keymap-bind-sequence! *global-keymap* '(#\M #\b #\b) 'vcalc-commands::set-binary-mode :str-base2)
  (keymap-bind-sequence! *global-keymap* '(#\M #\b #\o) 'vcalc-commands::set-octal-mode)
  (keymap-bind-sequence! *global-keymap* '(#\M #\b #\h) 'vcalc-commands::set-hexadecimal-mode)
  (keymap-bind-sequence! *global-keymap* '(#\M #\b #\d) 'vcalc-commands::set-decimal-mode)
  
  (keymap-describe-sequence! *global-keymap* '(#\M #\a) "Angle >>")
  (keymap-bind-sequence! *global-keymap* '(#\M #\a #\d) 'vcalc-commands::set-degree-mode)
  (keymap-bind-sequence! *global-keymap* '(#\M #\a #\r) 'vcalc-commands::set-radian-mode)
  (keymap-bind-sequence! *global-keymap* '(#\M #\a #\g) 'vcalc-commands::set-gradian-mode)

  (keymap-describe-sequence! *global-keymap* '(#\M #\n) "Number Format >>")
  (keymap-bind-sequence! *global-keymap* '(#\M #\n :key-space) 'vcalc-commands::set-no-seperator)
  (keymap-bind-sequence! *global-keymap* '(#\M #\n #\,) 'vcalc-commands::set-comma-seperator)
  (keymap-bind-sequence! *global-keymap* '(#\M #\n #\.) 'vcalc-commands::set-period-seperator)
  (keymap-bind-sequence! *global-keymap* '(#\M #\n #\s) 'vcalc-commands::set-scientific-mode)
  (keymap-bind-sequence! *global-keymap* '(#\M #\n #\f) 'vcalc-commands::set-fixed-mode)

  
  (keymap-describe-sequence! *global-keymap* '(#\k) "Constants >>")
  (keymap-bind-sequence! *global-keymap* '(#\k #\k) 'vcalc-commands::constant-library)
  (keymap-bind-sequence! *global-keymap* '(#\k #\g) 'vcalc-commands::phi-constant)
  (keymap-bind-sequence! *global-keymap* '(#\k #\e) 'vcalc-commands::e-constant)
  (keymap-bind-sequence! *global-keymap* '(#\k #\p) 'vcalc-commands::pi-constant)


  (keymap-bind-sequence! *global-keymap* '((:control :key-return)) 'vcalc-commands::push-data)

  (keymap-describe-sequence! *global-keymap* '(#\d) "Data >>")


  (keymap-describe-sequence! *global-keymap* '(#\d #\f) "Data File>>")
  (keymap-bind-sequence! *global-keymap* '(#\d #\f #\i) 'vcalc-commands::import-csv-file)
  (keymap-bind-sequence! *global-keymap* '(#\d #\f #\e) 'vcalc-commands::export-csv-file)

  (keymap-describe-sequence! *global-keymap* '(#\d #\a) "Apply Transformation >>")
  (keymap-bind-sequence! *global-keymap* '(#\d #\a #\x) 'vcalc-commands::data-log-x)
  (keymap-bind-sequence! *global-keymap* '(#\d #\a #\y) 'vcalc-commands::data-log-y)
  
  (keymap-describe-sequence! *global-keymap* '(#\d #\p) "Predict >>")
  (keymap-bind-sequence! *global-keymap* '(#\d #\p #\x) 'vcalc-commands::data-predx)
  (keymap-bind-sequence! *global-keymap* '(#\d #\p #\y) 'vcalc-commands::data-predy)
  (keymap-bind-sequence! *global-keymap* '(#\d #\p #\X) 'vcalc-commands::data-predx-consuming #f)
  (keymap-bind-sequence! *global-keymap* '(#\d #\p #\Y) 'vcalc-commands::data-predy-consuming #f)
  
  (keymap-bind-sequence! *global-keymap* '(#\d #\w) 'vcalc-commands::data-watch)
  (keymap-bind-sequence! *global-keymap* '(#\d (:control #\v)) 'vcalc-commands::data-pcov)
  (keymap-bind-sequence! *global-keymap* '(#\d #\v) 'vcalc-commands::data-cov)
  (keymap-bind-sequence! *global-keymap* '(#\d #\c) 'vcalc-commands::data-corr)
  (keymap-bind-sequence! *global-keymap* '(#\d #\l) 'vcalc-commands::data-lr)
  (keymap-bind-sequence! *global-keymap* '(#\d #\s) 'vcalc-commands::data-sdev)
  (keymap-bind-sequence! *global-keymap* '(#\d #\m) 'vcalc-commands::data-avg)
  (keymap-bind-sequence! *global-keymap* '(#\d #\t) 'vcalc-commands::data-tot)
  (keymap-bind-sequence! *global-keymap* '(#\d #\T) 'vcalc-commands::data-atot)
  (keymap-bind-sequence! *global-keymap* '(#\d #\D) 'vcalc-commands::clear-data)

  
  (keymap-describe-sequence! *global-keymap* '(#\n) "Number >>")
  (keymap-bind-sequence! *global-keymap* '(#\n #\i) 'vcalc-commands::->ieee-754-bits)
  (keymap-bind-sequence! *global-keymap* '(#\n #\I) 'vcalc-commands::ieee-754-bits->)
  (keymap-bind-sequence! *global-keymap* '(#\n #\r) 'vcalc-commands::round)
  (keymap-bind-sequence! *global-keymap* '(#\n #\t) 'vcalc-commands::truncate)
  (keymap-bind-sequence! *global-keymap* '(#\n #\f) 'vcalc-commands::floor)
  (keymap-bind-sequence! *global-keymap* '(#\n #\c) 'vcalc-commands::ceiling)
  (keymap-bind-sequence! *global-keymap* '(#\n #\a) 'vcalc-commands::abs)
  (keymap-bind-sequence! *global-keymap* '(#\n #\p) 'vcalc-commands::prng)
  (keymap-bind-sequence! *global-keymap* '(#\n #\P) 'vcalc-commands::prng-seed)


    
  (when #f ; Disabled, this doesn't work
    (keymap-describe-sequence! *global-keymap* '(#\t) "Time-Value of Money >>")

    (keymap-bind-sequence! *global-keymap* '(#\t #\T) 'vcalc-commands::tvm-solve-pmt "Solve PMT")
    (keymap-bind-sequence! *global-keymap* '(#\t #\t) 'vcalc-commands::tvm-set-pmt "PMT")
    (keymap-bind-sequence! *global-keymap* '(#\t #\I) 'vcalc-commands::tvm-solve-i "Solve i")
    (keymap-bind-sequence! *global-keymap* '(#\t #\i) 'vcalc-commands::tvm-set-i "i")
    (keymap-bind-sequence! *global-keymap* '(#\t #\N) 'vcalc-commands::tvm-solve-n "Solve n")
    (keymap-bind-sequence! *global-keymap* '(#\t #\n) 'vcalc-commands::tvm-set-n "n")
    (keymap-bind-sequence! *global-keymap* '(#\t #\F) 'vcalc-commands::tvm-solve-fv "Solve FV")
    (keymap-bind-sequence! *global-keymap* '(#\t #\f) 'vcalc-commands::tvm-set-fv "FV")
    (keymap-bind-sequence! *global-keymap* '(#\t #\P) 'vcalc-commands::tvm-solve-pv "Solve PV")
    (keymap-bind-sequence! *global-keymap* '(#\t #\p) 'vcalc-commands::tvm-set-pv "PV"))
  
  (keymap-describe-sequence! *global-keymap* '(#\m) "Math >>")
  (keymap-bind-sequence! *global-keymap* '(#\m #\g) 'vcalc-commands::gamma)
  (keymap-bind-sequence! *global-keymap* '(#\m #\E) 'vcalc-commands::erf)
  (keymap-bind-sequence! *global-keymap* '(#\m #\n) 'vcalc-commands::normal-dist)
  (keymap-bind-sequence! *global-keymap* '(#\m #\N) 'vcalc-commands::normal-dist-prob)

  (keymap-bind-sequence! *global-keymap* '(#\m (:control #\T)) 'vcalc-commands::atan2)
  (keymap-bind-sequence! *global-keymap* '(#\m #\S) 'vcalc-commands::asin)
  (keymap-bind-sequence! *global-keymap* '(#\m #\C) 'vcalc-commands::acos)
  (keymap-bind-sequence! *global-keymap* '(#\m #\T) 'vcalc-commands::atan)
  (keymap-bind-sequence! *global-keymap* '(#\m #\s) 'vcalc-commands::sin)
  (keymap-bind-sequence! *global-keymap* '(#\m #\c) 'vcalc-commands::cos)
  (keymap-bind-sequence! *global-keymap* '(#\m #\t) 'vcalc-commands::tan)

  (keymap-bind-sequence! *global-keymap* '(#\m #\l) 'vcalc-commands::log)
  (keymap-bind-sequence! *global-keymap* '(#\m #\L) 'vcalc-commands::exp)
  (keymap-bind-sequence! *global-keymap* '(#\m (:control #\l)) 'vcalc-commands::log10)
  (keymap-bind-sequence! *global-keymap* '(#\m (:control #\L)) 'vcalc-commands::exp10)
  
  (keymap-bind-sequence! *global-keymap* '(#\m #\!) 'vcalc-commands::factorial)

  (keymap-describe-sequence! *global-keymap* '(#\m #\%) "% >>")
  (keymap-bind-sequence! *global-keymap* '(#\m #\% #\%) 'vcalc-commands::percent)
  (keymap-bind-sequence! *global-keymap* '(#\m #\% #\d) 'vcalc-commands::percent-delta)
  (keymap-bind-sequence! *global-keymap* '(#\m #\% #\t) 'vcalc-commands::percent-t)

  (keymap-describe-sequence! *global-keymap* '(#\r) "Register >>")
  (keymap-bind-sequence! *global-keymap* '(#\r #\U) 'vcalc-commands::register-unwatch-all)
  (keymap-bind-sequence! *global-keymap* '(#\r #\u) 'vcalc-commands::register-unwatch)
  (keymap-bind-sequence! *global-keymap* '(#\r #\w) 'vcalc-commands::register-watch)
  (keymap-bind-sequence! *global-keymap* '(#\r #\x) 'vcalc-commands::rexch)
  (keymap-bind-sequence! *global-keymap* '(#\r #\r) 'vcalc-commands::rapply)
  (keymap-bind-sequence! *global-keymap* '(#\r #\R) 'vcalc-commands::rrecall)
  (keymap-bind-sequence! *global-keymap* '(#\r #\s) 'vcalc-commands::rstore)

  (keymap-bind-sequence! *global-keymap* '((:control #\x)) 'vcalc-commands::stack-cut #f)
  (keymap-bind-sequence! *global-keymap* '((:control #\c)) 'vcalc-commands::stack-copy #f)
  (keymap-bind-sequence! *global-keymap* '((:control #\v)) 'vcalc-commands::stack-paste #f)

  (keymap-bind-sequence! *global-keymap* '(#\~) 'vcalc-commands::bitwise-not #f)
  (keymap-bind-sequence! *global-keymap* '(#\&) 'vcalc-commands::bitwise-and #f)
  (keymap-bind-sequence! *global-keymap* '(#\|) 'vcalc-commands::bitwise-or #f)
  (keymap-bind-sequence! *global-keymap* '(#\^) 'vcalc-commands::bitwise-xor #f)
  (keymap-bind-sequence! *global-keymap* '(#\<) 'vcalc-commands::bitwise-shl #f)
  (keymap-bind-sequence! *global-keymap* '(#\>) 'vcalc-commands::bitwise-shr #f)

  (keymap-bind-sequence! *global-keymap* '(#\+) 'vcalc-commands::+ #f)
  (keymap-bind-sequence! *global-keymap* '(#\-) 'vcalc-commands::- #f)
  (keymap-bind-sequence! *global-keymap* '(#\*) 'vcalc-commands::* #f)
  (keymap-bind-sequence! *global-keymap* '(#\/) 'vcalc-commands::/ #f)

  (keymap-bind-sequence! *global-keymap* '((:shift :key-escape)) 'vcalc-commands::stack-clear)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-down)) 'vcalc-commands::srotd)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-up)) 'vcalc-commands::srot)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-return)) 'vcalc-commands::stack-dup2)
  (keymap-bind-sequence! *global-keymap* '(:key-return) 'vcalc-commands::stack-dup)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-back)) 'vcalc-commands::stack-dropn)
 (keymap-bind-sequence! *global-keymap* '(:key-back) 'vcalc-commands::stack-drop)
  (keymap-bind-sequence! *global-keymap* '(:key-tab) 'vcalc-commands::stack-swap)
  (keymap-bind-sequence! *global-keymap* '((:control #\a)) 'vcalc-commands::last-arguments)
  (keymap-bind-sequence! *global-keymap* '((:control #\y)) 'vcalc-commands::redo-stack)
  (keymap-bind-sequence! *global-keymap* '((:control #\z)) 'vcalc-commands::last-stack)
  (keymap-bind-sequence! *global-keymap* '(#\%) 'vcalc-commands::percent)
  (keymap-bind-sequence! *global-keymap* '(#\x) 'vcalc-commands::expt)
  (keymap-bind-sequence! *global-keymap* '(#\s) 'vcalc-commands::sqrt)
  (keymap-bind-sequence! *global-keymap* '(#\_) 'vcalc-commands::chs)
  (keymap-bind-sequence! *global-keymap* '(:key-space) 'vcalc-commands::chs #f)    
  (keymap-bind-sequence! *global-keymap* '(:key-f1) 'vcalc-commands::toggle-key-help)


  (keymap-bind-sequence! *global-keymap* '(:key-f2 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f3 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f4 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f5 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f6 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f7 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f8 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f9 ) 'vcalc-commands::rapply-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(:key-f10) 'vcalc-commands::rapply-with-last-keystroke #f)

  (keymap-bind-sequence! *global-keymap* '((:shift :key-f2 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f3 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f4 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f5 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f6 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f7 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f8 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f9 )) 'vcalc-commands::rstore-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:shift :key-f10)) 'vcalc-commands::rstore-with-last-keystroke #f)

  (keymap-bind-sequence! *global-keymap* '((:control :key-f2 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f3 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f4 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f5 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f6 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f7 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f8 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f9 )) 'vcalc-commands::rrecall-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '((:control :key-f10)) 'vcalc-commands::rrecall-with-last-keystroke #f)


  (keymap-bind-sequence! *global-keymap* '(#\@) 'vcalc-commands::toggle-console)
   
  (keymap-bind-sequence! *global-keymap* '(#\0) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\1) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\2) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\3) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\4) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\5) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\6) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\7) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\8) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\9) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\.) 'begin-editor-with-last-keystroke #f)
  (keymap-bind-sequence! *global-keymap* '(#\#) 'begin-editor-with-last-keystroke #f)

  *global-keymap*)
