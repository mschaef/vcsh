
;;;; autoload.scm --
;;;;
;;;; This package keeps a record of all loaded files and reloads
;;;; them when the file changes on disk.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "autoload"
  (:uses "scheme"))

;; The order in which files were initially loaded into the interpreter.
(define *loaded-file-order* ())

;; The last update times of currently loaded files.
(define *loaded-file-times* (make-hash))

;; A list of files updated by the current update operation. This is
;; #f if no update is currently under way.
(define *updated-files* #f)

(define (file-update-time filename)
  "Determines the last time the file named by <filenam> was updated.
   If there is no file named <filename>, returns #f."
  (if (file-exists? filename)
      (hash-ref (file-details filename) :write-time)
      #f))

(define (notice-load filename)
  "If <filename> refers to a file that exists, make a record of the
   the fact that it is to be loaded."
  (when (file-exists? filename)
    (when *updated-files*
      (push! filename *updated-files*))
    (unless (member filename *loaded-file-order*)
      (set! *loaded-file-order* (append *loaded-file-order* (list filename))))
    (hash-set! *loaded-file-times* filename (file-update-time filename))))

(define (more-recent-date? x y)
  "Returns the date <x> if it is more recent than the date <y>."
  (if (time>? (date->time-utc x)
              (date->time-utc y))
      x
      #f))

(define (update-necessary? filename)
  "Determines if it is necessary to reload the file named by <filename>,
   to keep the currently running interpreter up to date with currently
   loaded source files. For this to be true, the file needs to exist on
   disk and the file on disk needs to have been updated after the most
   recent load. Files that have not been yet beenloaded are deemed to need
   reloading, by default. "
  (and (file-exists? filename)
       (or (not (member filename *loaded-file-order*))
           (more-recent-date? (file-update-time filename)
                              (hash-ref *loaded-file-times* filename)))))

(define *automatic-load-enabled* #t)

(define (update-loaded-files :optional (force? #f))
  "Scan the list of currently loaded files, reloading any that
   have been updated on disk. If <force?> is true, files are
   always reloaded."
  (dynamic-let ((*updated-files* ()))
    (dolist (filename *loaded-file-order*)
      (when (and (or force?
                     ;; We always load if the user has requested a forced
                     ;; load. Otherwise, we only update automatically if
                     ;; the file has been updated on disk and automatic loads
                     ;; are are enabled.
                     (and *automatic-load-enabled*
                          (update-necessary? filename)))
                 ;; If we've already updated the file, skip it. This avoids
                 ;; reloading files that are loaded by explicit loads in
                 ;; source files.
                 (or (not *updated-files*)
                     (not (member filename *updated-files*))))
        (info "Reloading file: ~a" filename)
        (load filename)))))


(define (show-loaded-files)
  (format (current-error-port) "~& Currently Loaded Files:\n")
  (format (current-error-port) "~&------------------------------------------------\n")
  (dolist (filename *loaded-file-order*)
    (format (current-error-port) "~&~2T~a~20T@~a\n" filename (hash-ref *loaded-file-times* filename))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-hook-function! '*pre-load-hook* 'notice-load)
  (add-hook-function! '*repl-pre-read-hook* 'update-loaded-files))

(define-repl-abbreviation :update update-loaded-files)
(define-repl-abbreviation :loaded show-loaded-files)
