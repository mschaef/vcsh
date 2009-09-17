;;;; system.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;
;; OS-specific interface functions


(define (system-info :optional (attribute #f))
  "Retrieve information about the attributes of the current system environment.  If <attribute>
   is #f or unspecified a hash of all attribute is returned. If <attribute> is specified and a valid
   attribute name, the attribute value is returned, otherwise a runtime error is signaled."
  (let ((all (%system-info)))
    (cond ((not attribute)
           all)
          ((not (hash-has? all attribute))
           (error "Unknown system-info attribute: ~s" attribute))
          (#t
           (hash-ref all attribute)))))

(define *cached-platform* #f)

(defmacro (platform-case . case-clauses)
  "A variant of case used to pick between branches of code based on the
   currently running platform.  The car of each case clause specifies
   the platform(s) on which the body of that case clause will run.  The
   car can either be one platform name or a list of platform names. The
   platform name is the second item of the list returned by (system-info)."
  `(begin
     (unless *cached-platform*
       (set! *cached-platform* (system-info :platform-name)))
     (case *cached-platform*
       ,@case-clauses
       (#t (error "Unsupported platform: ~s" *cached-platform*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Runtime Environment

(define *environment-vars* #f)

(define (environment-variable name)
  (unless *environment-vars*
    (set! *environment-vars*
          (map (lambda (binding)
                 (cons (string-downcase (car binding)) (cdr binding)))
               (environment))))
  (aif (assoc (string-downcase name) *environment-vars*)
       (cdr it)
       #f))

(define *current-load-file* #f)

(define (path-quote-char? ch)
  "Returns <ch> if it is a quoting character in pathnames on the
  current platform, #f otherwise. Quoting characters are those that
  cause any special interpretation of the subsequent character to be
  ignored. On Linux, #\\\\ is a quoting character. There is no such
  character on Windows."
  (platform-case ((:win32) #f)
                 (#t (char=? ch #\\))))

(define (path-delimiter-char? ch)
  "Returns <ch> if it is a path delimiting character in pathnames on the
  current platform, #f otherwise."
  (platform-case ((:win32) (or (char=? ch #\\)
                               (char=? ch #\/)))
                 ((:linux) (char=? ch #\/))))

(define (path-string->list str)
  "Parse a directory path string, returning a list of each directory in the path
   in the order in which it appears. Each directory in the path is seperated by
   either a #\\: or #\\;. These may appear in a directory name itself if quoted
   with a #\\\\ (except on Windows). Empty directory names are eliminated from
   the resultant list."
  (let ((ip (open-input-string str)))
    (let loop ((dirs ())
               (op (open-output-string)))
      (define (current-dir)
        (let ((dir-str (get-output-string op)))
          (if (= 0 (length dir-str))
              ()
              dir-str)))
      (cond ((port-at-end? ip)
             (reverse! (remove null? (cons (current-dir) dirs))))
            ((or (char=? (peek-char ip) #\;)
                 (char=? (peek-char ip) #\:))
             (read-char ip)
             (loop (cons (current-dir) dirs) (open-output-string)))
            (#t
             ;; The windows directory seperator can't be used to quote things, since
             ;; it has higher callings on Windows.
             (when (path-quote-char? (peek-char ip))
               (read-char ip))
             (write-char (read-char ip) op)
             (loop dirs op))))))

;;;; Filename utilities

(define (home-directory)
  "Returns the path to the current user's home directory. Returns #f if
   the home directory cannot be determined."
  (aif (platform-case
        ((:win32)
         (let ((d (environment-variable "HOMEDRIVE"))
               (p (environment-variable "HOMEPATH")))
           (acond ((and d p)
                   (string-append d p))
                  ((environment-variable "USERPROFILE")
                   it)
                  (#t
                   #f))))
        ((:linux)
         (aif (environment-variable "HOME")
              it
              #f)))
       (filename-append-delimiter it)
       it))


(define (filename-string=? x y)
  "Compare two filenames <x> and <y>, using system-specific conventions
   for filename comparison. On Unix, this means case-sensitive comparison
   on Win32, this means case-insensitive."
  (or (eq? x y)
      (and (string? x)
           (string? y)
           (platform-case ((:win32) (string=-ci x y))
                          ((:linux) (string= x y))))))

(define (filename-path-delimiter-pos filename)
  (platform-case
   ((:win32) (or (string-search #\/ filename)
                 (string-search #\\ filename)))
   ((:linux) (string-search #\/ filename))))


(define (filename-last-path-delimiter-pos filename)
  (platform-case
   ((:win32) (or (string-search-from-right #\/ filename)
                 (string-search-from-right #\\ filename)))
   ((:linux) (string-search-from-right #\/ filename))))

(define (filename-first/rest filename) ; REVISIT: possibly better as filename-fold
  "Splits <filename> at the first directory delimiter. The first value
   returned is the first section of the filename, the second value is
   the rest."
  (let ((first-delim (filename-path-delimiter-pos filename)))
    (if first-delim
        (values (substring filename 0 (+ first-delim 1))
                (substring filename (+ first-delim 1)))
        (values filename
                ""))))

(define (is-directory-basename? basename)
  "Determines if <basename> refers to a directory, as opposed to a file.
    This only considers the text of the basename, not the contents of
    the filesystem."
  (if (or (member basename '("." ".." :relative :absolute :back :same-dir
                             :delim :any-dirs))
          (and (string? basename)
               (> (string-length basename) 0)
               (path-delimiter-char? (string-ref basename (- (string-length basename) 1)))))
      basename
      #f))

(define (is-directory-filename? filename)
  "Determines if <filename> refers to a directory, as opposed to a file.
   This only considers the text of the basename, not the contents of the
   filename."
  (is-directory-basename? (last (filename->list filename))))

(define (is-file-basename? basename)
  "Determines if <baseame> refers to a file, as opposed to a directory.
   This only considers the text of the basename, not the contents of the
   basename."
  (if (and (string? basename)
           (not (is-directory-basename? basename)))
      basename
      #f))

(define (is-file-filename? filename)
  "Determines if <filename> refers to a file, as opposed to a directory.
   This only considers the text of the basename, not the contents of the
   filename."
  (is-file-basename? (last (filename->list filename))))

(define *filename-delimiter* #\/)

(define (make-filename . filenames)
  "Concatenate <filenames> into a properly delimited filename."
  (let ((op (open-output-string)))
    (fold (lambda (name needs-seperator?)
            (cond ((and (string? name) (> (length name) 0))
                   (when needs-seperator?
                     (display *filename-delimiter* op))
                   (display name op)
                   (not (char=? (string-ref name (- (length name) 1)) #\/)))
                  (#t
                   needs-seperator?)))
          #f
          filenames)
    (get-output-string op)))

(define (filename-append-delimiter filename)
  "Appends a filename delimiter to <filename>, with no regard to whether or not
   the filename already has a trailing delimiter."
  (string-append filename *filename-delimiter*))

(define (filename-prepend-delimiter filename)
  "Prepends a filename delimiter to <filename>, with no regard to whether or not
   the filename already has a leading delimiter."
  (string-append *filename-delimiter* filename))

(define (filename-strip-trailing-delimiter filename)
  "Removes the trailing filename delimimter from <filename>, if it has one. Returns
   <filename>, otherwise."
  (if (path-delimiter-char? (string-ref filename (- (length filename) 1))) ; TODO: string-first, string-last
      (substring filename 0 (- (length filename) 1))
      filename))

(define (list->filename filename-list)
  "Convert <filename-list> into a string representation of the filename. The input
   list has one element  sub-components. Each sub-component is either a string
   containing a directory or filename, or a keyword signifying a component  with
   a special meaning. Directory strings are distinguished by a trailing directory
   delimimter. The possible keywords are :back, :same-dir, :any-dirs,  :absolute,
   or :delim."
  (apply make-filename (map (lambda (fn-component)
                              (case fn-component
                                ((:delim :same-dir) #f)
                                ((:back) (filename-append-delimiter ".."))
                                ((:any-dirs) (filename-append-delimiter "**"))
                                ((:absolute) (filename-append-delimiter ""))
                                ((:relative) "")
                                (#t
                                 (unless (string? fn-component)
                                   (error "Invalid filename component ~s in filename list ~s."
                                          fn-component
                                          filename-list))
                                 fn-component)))
                            filename-list)))

(define (filename->list filename)
  "Split <filename> into a list of sub-components. Each sub-component is either
    a string containing a directory or filename, or a keyword signifying a component
    with a special meaning. Directory strings are distinguished by a a trailing
    directory delimimter. The possible keywords are :back, :same-dir, :any-dirs,
    :absolute, or :delim."
  (let loop ((filename (string-trim filename))
             (xs ()))
    (if (string-empty? filename)
        (if (null? xs)
            '(:relative)
            (reverse xs))
        (values-bind (filename-first/rest filename) (fn-first fn-rest)
          (let ((fn-first/nd (filename-strip-trailing-delimiter fn-first)))
            (cond ((null? xs)
                   (if (filename-string=? fn-first/nd "")
                       (loop fn-rest (cons :absolute xs))
                       (loop filename (cons :relative xs))))
                  ((filename-string=? fn-first/nd  "..")
                   (cond ((eq? (car xs) :absolute)
                          (error "Attempt to ascend the root of an absolute path. ~s" filename))
                         ((and (not (member (car xs) '(:absolute :relative :back :same-dir :any-dirs)))
                               (is-directory-basename? (car xs))
                               (is-filename-exact? (car xs)))
                          (loop fn-rest (cdr xs)))
                         (#t
                          (loop fn-rest (cons :back xs)))))
                  ((filename-string=? fn-first/nd ".")
                   (loop fn-rest xs))
                  ((and (filename-string=? fn-first/nd "*") (not (equal? fn-first fn-first/nd)))
                   (loop fn-rest (cons :any-dir xs)))
                  ((and (filename-string=? fn-first/nd  "**") (not (equal? fn-first fn-first/nd)))
                   (loop fn-rest (cons :any-dirs xs)))
                  ((filename-string=? fn-first/nd "**")
                   (loop fn-rest `("*" :any-dirs ,@xs)))
                  ((filename-string=? fn-first/nd "")
                   (loop fn-rest xs))
                  (#t
                   (loop fn-rest (cons fn-first xs)))))))))

(define (canonicalize-filename filename)
  "Reduce <filename> into canonical form. All duplicate path delimiters
   are removed, as are redundant path information. This process leaves
   the simplest representation of the input filename."
  (list->filename (filename->list filename)))

(define (filename-maybe-add-relative-prefix filename)
  (if (and (> (string-length filename) 0)
           (member (string-ref filename 0) (platform-case ((:win32) '(#\. #\/ #\\))
                                                          ((:linux)  '(#\. #\/)))))
      filename
      (string-append "./" filename)))

(define (filename-path filename)
  "Determines the path name part of <filename>."
  (filename-maybe-add-relative-prefix
   (let ((filename (canonicalize-filename filename)))
     (if (is-directory-filename? filename)
         filename
         (aif (filename-last-path-delimiter-pos filename)
              (string-take filename (+ it 1))
              "")))))

(define (filename-basename filename)
  "Determines the name of the file referred to by <filename>, not including
   the directory. If <filename> refers to a directory, this function returns
   #f."
  (let ((basename (last (filename->list filename))))
    (if (or (not (string? basename))
            (is-directory-basename? basename))
        #f
        basename)))

(define (filename-extension filename)
  "Determines the extension of the file referred to by <filename>. If <filename>
  refers to a directory, returns #f."
  (let ((basename (filename-basename filename)))
    (if basename
        (aif (string-search-from-right #\. basename)
             (string-drop basename (+ 1 it))
             "")
        #f)))

(define (filename-no-extension filename)
  "Removes the extension, if any, from filename. If <filename> refers to a directory,
   returns #f"
  (if (is-directory-filename? filename)
      #f
      (aif (string-search-from-right #\. filename)
           (string-take filename it)
           filename)))

(define (is-filename-exact? filename)
  "Returns <filename> if it's exact and can only match one file, returns #f
   if <filename> can match multiple files."
  (not (or (eq? filename :any-dirs)
           (wild-glob-pattern? filename))))

;;;; Filename globbing

(define (parse-glob-pattern basename) ; REVISIT: Add quoting for *? -- bash seems to do this
  (define (any-1? x) (eq? x :any-1))
  (let loop ((basename basename) (basename-sections ()))
    (acond
     ((= (length basename) 0) ; REVISIT: this depends on length returing 0 for :relative... odd
      (reverse! basename-sections))
     ((char=? #\* (string-ref basename 0))
      (loop (substring basename 1)
            (case (car basename-sections)
              ((:any-n) basename-sections)
              ((:any-1) (cons :any-n (drop-while any-1? basename-sections)))
              (#t (cons :any-n basename-sections)))))
     ((char=? #\? (string-ref basename 0))
      (loop (substring basename 1)
            (case (car basename-sections)
              ((:any-n) basename-sections)
              (#t (cons :any-1 basename-sections)))))
     ((string-first-character basename "*?")
      (loop (substring basename it) (cons (substring basename 0 it) basename-sections)))
     (#t
      (loop "" (cons basename basename-sections))))))

(define (wild-glob-pattern? pattern)
  "Returns <pattern> if it is a glob pattern with wildcards, #f otherwise."
  (if (any? symbol? (parse-glob-pattern pattern))
      pattern
      #f))

(define (glob-matcher pattern)
  (let ((pattern-list (parse-glob-pattern pattern)))
    (lambda (filename)
      (let ((last-char (- (length filename) 1)))
        (let loop ((pattern-list pattern-list)
                   (loc 0))
          (cond ((null? pattern-list)
                 (= loc (+ 1 last-char)))
                ((eq? (car pattern-list) :any-1)
                 (loop (cdr pattern-list) (+ loc 1)))
                ((eq? (car pattern-list) :any-n)
                 ;; search for the next token in the pattern, if there is no next token,
                 ;; then the :any-n consumes the rest of the filename and we pass by default.
                 (if (null? (cdr pattern-list))
                     filename
                     (aif (string-search (cadr pattern-list) filename loc)
                          ;; This is a bit odd... basically, we need to handle the case where
                          ;; the 'next' token occurs more than once. If this is true,
                          ;; and we fail to match the filename when we try the first instance
                          ;; of the token, we should try successive instances of the token.
                          (if (loop (cddr pattern-list) (+ (length (cadr pattern-list)) it))
                              filename
                              (loop pattern-list (+ loc 1)))
                          #f)))
                (#t
                 ;; search for token at loc, fail if not found exactly there
                 (let ((string-found-at (string-search (car pattern-list) filename loc)))
                   (if (and string-found-at (= string-found-at loc))
                       (loop (cdr pattern-list) (+ (length (car pattern-list)) loc))
                       #f)))))))))

(define (matches-glob? filename pattern)
  "Returns <filename>, if it matches the glob pattern <pattern>,
   returns #f otherwise."
  (if ((glob-matcher pattern) filename)
      filename
      #f))

(define (filter-by-glob filenames pattern)
  "Returns a list of only the names in <filenames> that globs against
   <pattern>."
  (let ((matches-pattern? (glob-matcher pattern)))
    (filter matches-pattern? filenames)))

;;;; directory listing

(define (file-details filename :optional (existance-only? #f))
  (let ((details (%file-details filename existance-only?)))
    (unless existance-only?
      (hash-set! details :write-time (tm%realtime->date (hash-ref details :write-time)))
      (hash-set! details :create-time (tm%realtime->date (hash-ref details :create-time)))
      (hash-set! details :access-time (tm%realtime->date (hash-ref details :access-time))))
    details))


(define (%directory-with-full-paths exact-directory-name mode :optional (include? (always #t)))
  (map #L(make-filename exact-directory-name _)
       (filter include? (%directory exact-directory-name mode))))

(define (not-dot-directory? filename)
  (not (platform-case
        ((:win32)
         (member filename '("./" "../" ".\\" "..\\")))
        ((:linux)
         (member filename '("./" "../"))))))

(define (directory filename)
  (let ((filename (if (string? filename)
                      (filename->list filename)
                      filename)))
    (values-bind (span is-filename-exact? filename) (exact wild)
      (let ((exact-filename (list->filename exact)))
        (cond ((null? wild)
               (%directory-with-full-paths exact-filename :all))
              ((eq? (car wild) :any-dirs)
               (append! (directory (list->filename (cons exact-filename (cdr wild))))
                        (append-map! (lambda (exact-directory-filename)
                                       (directory (list->filename (cons exact-directory-filename wild))))
                                     (%directory-with-full-paths (list->filename exact)
                                                                 :directories
                                                                 not-dot-directory?))))
              ((is-file-basename? (car wild))
               (%directory-with-full-paths exact-filename :files (glob-matcher (car wild))))
              ;; REVISIT: this is where directory wildcards would go /*foo/bar.cpp
              )))))


