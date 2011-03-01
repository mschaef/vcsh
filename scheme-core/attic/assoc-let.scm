
;;;; assoc-let.scm --
;;;;
;;;; Let forms for looking up items in association lists. These were in the
;;;; image for years, and never used.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(defmacro (%a-list-search-let let-form a-list-search-form a-list bindings . body-forms)
  (define (binding-form binding)
    (let ((variable (first binding))
          (key (second binding))
          (default-value (third binding)))
      `(,variable (aif (,a-list-search-form ,key ,a-list) (cdr it) ,default-value))))
  (dolist (binding bindings)
    (unless (and (list? binding)
                 (symbol? (car binding))
                 (or (= 3 (length binding))
                     (= 2 (length binding))))
      (error "Invalid binding in assoc-let: ~a" binding)))
  `(,let-form ,(map binding-form bindings) ,@body-forms))

(defmacro (assoc-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assoc to search <a-list>."
  `(%a-list-search-let let assoc ,a-list ,bindings ,@body-forms))

(defmacro (ass-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses ass to search <a-list>."
  `(%a-list-search-let let ass ,a-list ,bindings ,@body-forms))

(defmacro (assq-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assq to search <a-list>."
  `(%a-list-search-let let assq ,a-list ,bindings ,@body-forms))

(defmacro (assv-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assv to search <a-list>."
  `(%a-list-search-let let assv ,a-list ,bindings ,@body-forms))

(defmacro (assoc-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assoc to search <a-list>."
  `(%a-list-search-let let* assoc ,a-list ,bindings ,@body-forms))

(defmacro (ass-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses ass to search <a-list>."
  `(%a-list-search-let let* ass ,a-list ,bindings ,@body-forms))

(defmacro (assq-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assq to search <a-list>."
  `(%a-list-search-let let* assq ,a-list ,bindings ,@body-forms))

(defmacro (assv-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assv to search <a-list>."
  `(%a-list-search-let let* assv ,a-list ,bindings ,@body-forms))

(defmacro (assoc-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assoc to search <a-list>."
  `(%a-list-search-let letrec assoc ,a-list ,bindings ,@body-forms))

(defmacro (ass-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses ass to search <a-list>."
  `(%a-list-search-let letrec ass ,a-list ,bindings ,@body-forms))

(defmacro (assq-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assq to search <a-list>."
  `(%a-list-search-let letrec assq ,a-list ,bindings ,@body-forms))

(defmacro (assv-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assv to search <a-list>."
  `(%a-list-search-let letrec assv ,a-list ,bindings ,@body-forms))
