;;;; lex.lisp - The lexical analyzer for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
; rev 1 - Initial Release
;
; rev 2 - Modifications for syn release
;  * Moved common code out to utility.lisp
;  * Changed to phase-specific error handlers
;  * Eliminated test case code
;  * fixed typographical error involving close-parems :)
;
; rev 3 - Modifications for sem release
;  * Added code to distinguish between integer and float lexemes
;
; rev 4 - Final 375 Release
;  * Added code to generate literal ast list

;;; A handler for lexical errors

(define (lex-error error-message . paramaters)
  "Function call to issue a lexical error"
  (apply compiler-error 'lexical :no-line-number error-message paramaters))

;;; Global variables maintained by the lexical analyxer

(define *lexeme-dictionary*
  (make-hash :equal)
  "A map assosciating strings found in the lexeme string with symbols")

(define *character-class*
  (make-vector 256 #f)
  "An array storing a binding to a character class for each ASCII character")

;;; Code for lexeme bindings

(define (add-standard-token! lexeme lexeme-class)
  "Add a binding from a lexeme to a Lisp object"
  (hash-set! *lexeme-dictionary* lexeme lexeme-class))

(define (standard-token-class lexeme)
  "Returns the standard token class of <lexeme>, #f is there is no standard
   class for the token."
  (hash-ref *lexeme-dictionary* lexeme #f))

(define (get-literal-lexeme-ast )
  "Return a list of literal lexeme nodes"
  (let (ast-nodes)
    (maphash (lambda (key val)
               (if (member val '(string-literal
                                 character-literal
                                 float-literal
                                 integer-literal
                                 hex-literal
                                 octal-literal))
                   (push
                    (let ((node (make-node val)))
                      (set-node-attribute node 'name key))
                    ast-nodes)))
             *lexeme-dictionary*)
    ast-nodes))

;;; An array used to specify classes of characters

(define (set-character-class! ch class)
  "Set the class of the specified character."
  (vector-set! *character-class* (char->integer ch) class))

(define (get-character-class ch)
  "Return the character binding of a character, #f if unbound or end of file.."
  (if (eof-object? ch)
      #f
      (vector-ref *character-class* (char->integer ch))))

;;; Token utility functions

(define-structure token
  class
  (line :default #f)
  info)


;;; The actual lexical analysis code

(define (skip-multiline-comment input-stream)
  "read to the end of a multiple line comment started by /* (or /**)"
  (while (not (and (eq? (read-char input-stream) #\*)
                   (eq? (peek-char input-stream) #\/)
                   (eq? (read-char input-stream) #\/)))
    ))

(define (skip-singleline-comment input-stream)
  "read to the end of a single line comment started by //"
  (read-line input-stream))

(define (skip-whitespace input-stream)
  "skip whitespace in an input stream"
  (while (eq? (get-character-class (peek-char input-stream)) 'whitespace)
    (read-char input-stream)))

(define (scan-characters input-stream valid-characters)
  "Scan the input stream as long as one of a list of characters is next"
  (let ((lexeme (open-output-string)))
    (while (memq (peek-char input-stream) valid-characters)
      (display (read-char input-stream) lexeme))
    (get-output-string lexeme)))

(define (scan-binary input-stream)
  "Scan a sequence of binary digits"
  (scan-characters input-stream '(#\0 #\1)))

(define (scan-octal input-stream)
  "Scan a sequence of octal digits"
  (scan-characters input-stream '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))

(define (scan-decimal input-stream)
  "Scan a sequence of decimal digits"
  (scan-characters input-stream '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define (scan-hex input-stream)
  "Scan a sequence of hexadecimal digits"
  (scan-characters input-stream '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                  #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E
                                  #\f #\F)))

(define (read-string-char input-stream)
  "read in a character, processing C-style escapes."
  (let ((first-character (read-char input-stream)))
    (if (eq? first-character #\\)
        (case (read-char input-stream)
          ((#\n) #\newline)
          ((#\t) #\tab)
          ((#\f) #\formfeed)
          ((#\b) #\bs)
          ((#\r) #\cr)
          ((#\\) #\\)
          ((#\') #\')
          ((#\") #\")
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
           (lex-error "Octal escapes currently unimplemented"))
          ((#\u #\U)
           (lex-error "Unicode escapes not supported"))
          (#t
           (lex-error "Invalid escape")))
        first-character)))

(define (get-string-or-char-token input-stream)
  "Read a string or character literal token"
  (case (read-char input-stream)
    ((#\')
     (let ((character (read-string-char input-stream)))
       (if (eq? (read-char input-stream) #\')
           (make-token :info (string-append "'"  character "'")
                       :class 'character-literal)
           (lex-error "error, unterminated character literal"))))
    ((#\")
     (let ((current-string ""))
       (while (not (eq? (peek-char input-stream) #\"))
         (set! current-string
               (string-append current-string
                              (read-string-char input-stream))))
       (read-char input-stream)
       (make-token :info (string-append "\"" current-string "\"")
                   :class 'string-literal)))
    (#t 
     (lex-error "Internal compiler error"))))

(define (scan-decimal-literal input-stream)
  "Scan a decimal numeric literal, handling fractional components, and exponents"
  (let ((lexeme (scan-decimal input-stream)) 
        (floating #f))
    (when (eq? (peek-char input-stream) #\.)
      (set! floating #t)
      (set! lexeme (string-append lexeme (read-char input-stream)))
      (set! lexeme (string-append lexeme (scan-decimal input-stream)))
      (if (eq? (char-downcase (peek-char input-stream)) #\l)
          (set! lexeme (string-append lexeme (read-char input-stream)))))
    (when (and (not (port-at-end? input-stream))
               (eq? (char-downcase (peek-char input-stream)) #\e))
      (set! lexeme (string-append lexeme (read-char input-stream)))
      (set! lexeme (string-append lexeme (scan-decimal input-stream))))
    (when (and (not (port-at-end? input-stream)) 
               (eq? (char-downcase (peek-char input-stream)) #\f))
      (set! lexeme (string-append lexeme (read-char input-stream))))
    (values lexeme floating)))

(define (get-numeric-token input-stream)
  "Get a numeric token, detecting octal and hex constants"

  (case (peek-char input-stream)
    ((#\0)
     (read-char input-stream)
     (case (peek-char input-stream)
       ((#\x #\X)
        (read-char input-stream)
        (make-token :info (string-append "0x" (scan-hex input-stream))
                    :class 'hex-literal))
       (#t
        (make-token :info (string-append "0" (scan-octal input-stream))
                    :class 'octal-literal))))
    (#t
     (values-bind (scan-decimal-literal input-stream) (lexeme floating-lexeme)
       (if floating-lexeme
           (make-token :info lexeme :class 'float-literal)
           (make-token :info lexeme :class'integer-literal))))))


(define (get-special-token input-stream)
  "Read a token made up of symbols"
  (let ((lexeme ""))

    ;; The conditional for this while loop basically says continue as long
    ;; as the next character is a symbol, and adding it to the existing lexeme
    ;; will produce a valid token
    (while (and (eq? (get-character-class (peek-char input-stream)) 'special)
                (standard-token-class (string-append lexeme  (peek-char input-stream))))
      (set! lexeme (string-append lexeme (read-char input-stream))))
    
    ;; This is nasty coding style, but it checks to see if the token retrieved
    ;; was one of the delimiters for a comment.  The delimiters are initialized
    ;; into the table of lexemes at the start of execution
    (cond
     ((equal? lexeme "//")
      (skip-singleline-comment input-stream)
      (get-next-token input-stream))
     ((equal? lexeme "/*")
      (skip-multiline-comment input-stream)
      (get-next-token input-stream))
     (#t
      (make-token :info lexeme
                  :class (standard-token-class lexeme))))))

(define (get-identifier-token input-stream)
  "Read an identifier token from the input stream, and return a token"
  (let ((lexeme ""))
    (while (member (get-character-class (peek-char input-stream))
                   '(lowercase uppercase numeric id))
      (set! lexeme (string-append lexeme (read-char input-stream))))
    (make-token :info lexeme 
                :class (aif (standard-token-class lexeme) it 'identifier))))

(define (get-next-token input-stream)
  "Retrieve the next token from the input stream, using the first character
   to distinguish between token types"
  (skip-whitespace input-stream)
  (let* ((location (port-location input-stream))
         (token (case (get-character-class (peek-char input-stream))
                  ((quotes)                 (get-string-or-char-token input-stream))
                  ((special)                (get-special-token input-stream))
                  ((numeric)                (get-numeric-token input-stream))
                  ((lowercase uppercase id) (get-identifier-token input-stream))
                  ((#f)                     (make-token :class 'end-of-file))
                  (#t (lex-error "Invalid character ~a" (peek-char input-stream))))))
    (set-token-line! token location)))

(define (install-standard-tables!)
  "Clear all bindings, and restore the bindings for reserved words"
  (set-character-class! #\a 'lowercase)
  (set-character-class! #\b 'lowercase)
  (set-character-class! #\c 'lowercase)
  (set-character-class! #\d 'lowercase)
  (set-character-class! #\e 'lowercase)
  (set-character-class! #\f 'lowercase)
  (set-character-class! #\g 'lowercase)
  (set-character-class! #\h 'lowercase)
  (set-character-class! #\i 'lowercase)
  (set-character-class! #\j 'lowercase)
  (set-character-class! #\k 'lowercase)
  (set-character-class! #\l 'lowercase)
  (set-character-class! #\m 'lowercase)
  (set-character-class! #\n 'lowercase)
  (set-character-class! #\o 'lowercase)
  (set-character-class! #\p 'lowercase)
  (set-character-class! #\q 'lowercase)
  (set-character-class! #\r 'lowercase)
  (set-character-class! #\s 'lowercase)
  (set-character-class! #\t 'lowercase)
  (set-character-class! #\u 'lowercase)
  (set-character-class! #\v 'lowercase)
  (set-character-class! #\w 'lowercase)
  (set-character-class! #\x 'lowercase)
  (set-character-class! #\y 'lowercase)
  (set-character-class! #\z 'lowercase)

  (set-character-class! #\A 'uppercase)
  (set-character-class! #\B 'uppercase)
  (set-character-class! #\C 'uppercase)
  (set-character-class! #\D 'uppercase)
  (set-character-class! #\E 'uppercase)
  (set-character-class! #\F 'uppercase)
  (set-character-class! #\G 'uppercase)
  (set-character-class! #\H 'uppercase)
  (set-character-class! #\I 'uppercase)
  (set-character-class! #\J 'uppercase)
  (set-character-class! #\K 'uppercase)
  (set-character-class! #\L 'uppercase)
  (set-character-class! #\M 'uppercase)
  (set-character-class! #\N 'uppercase)
  (set-character-class! #\O 'uppercase)
  (set-character-class! #\P 'uppercase)
  (set-character-class! #\Q 'uppercase)
  (set-character-class! #\R 'uppercase)
  (set-character-class! #\S 'uppercase)
  (set-character-class! #\T 'uppercase)
  (set-character-class! #\U 'uppercase)
  (set-character-class! #\V 'uppercase)
  (set-character-class! #\W 'uppercase)
  (set-character-class! #\X 'uppercase)
  (set-character-class! #\Y 'uppercase)
  (set-character-class! #\Z 'uppercase)

  (set-character-class! #\_ 'id)

  (set-character-class! #\1 'numeric)
  (set-character-class! #\2 'numeric)
  (set-character-class! #\3 'numeric)
  (set-character-class! #\4 'numeric)
  (set-character-class! #\5 'numeric)
  (set-character-class! #\6 'numeric)
  (set-character-class! #\7 'numeric)
  (set-character-class! #\8 'numeric)
  (set-character-class! #\9 'numeric)
  (set-character-class! #\0 'numeric)


  (set-character-class! #\{ 'special)
  (set-character-class! #\} 'special)
  (set-character-class! #\[ 'special)
  (set-character-class! #\] 'special)
  (set-character-class! #\( 'special)
  (set-character-class! #\) 'special)
  (set-character-class! #\, 'special)
  (set-character-class! #\; 'special)
  (set-character-class! #\> 'special)
  (set-character-class! #\< 'special)
  (set-character-class! #\^ 'special)
  (set-character-class! #\% 'special)
  (set-character-class! #\: 'special)
  (set-character-class! #\| 'special)
  (set-character-class! #\+ 'special)
  (set-character-class! #\- 'special)
  (set-character-class! #\/ 'special)
  (set-character-class! #\* 'special)
  (set-character-class! #\! 'special)
  (set-character-class! #\~ 'special)
  (set-character-class! #\? 'special)
  (set-character-class! #\. 'special)
  (set-character-class! #\= 'special)
  (set-character-class! #\& 'special)

  (set-character-class! #\' 'quotes )
  (set-character-class! #\" 'quotes )

  (set-character-class! #\space 'whitespace )
  (set-character-class! #\tab 'whitespace )
  (set-character-class! #\newline 'whitespace )


  (hash-clear!  *lexeme-dictionary*)

  ;; Bindings for reserved words
  (add-standard-token! "abstract" 'abstract)
  (add-standard-token! "default" 'default)
  (add-standard-token! "if" 'if)
  (add-standard-token! "private" 'private)
  (add-standard-token! "throw" 'throw)
  (add-standard-token! "boolean" 'boolean)
  (add-standard-token! "do" 'do)
  (add-standard-token! "implements" 'implements)
  (add-standard-token! "protected" 'protected)
  (add-standard-token! "throws" 'throws)
  (add-standard-token! "break" 'break)
  (add-standard-token! "double" 'double)
  (add-standard-token! "import" 'import)
  (add-standard-token! "public" 'public)
  (add-standard-token! "transient" 'transient)
  (add-standard-token! "byte" 'byte)
  (add-standard-token! "else" 'else)
  (add-standard-token! "instanceof" 'instanceof)
  (add-standard-token! "return" 'return)
  (add-standard-token! "try" 'try)
  (add-standard-token! "case" 'case)
  (add-standard-token! "extends" 'extends)
  (add-standard-token! "int" 'int)
  (add-standard-token! "short" 'short)
  (add-standard-token! "void" 'void)
  (add-standard-token! "catch" 'catch)
  (add-standard-token! "final" 'final)
  (add-standard-token! "interface" 'interface)
  (add-standard-token! "static" 'static)
  (add-standard-token! "volatile" 'volatile)
  (add-standard-token! "char" 'char)
  (add-standard-token! "finally" 'finally)
  (add-standard-token! "long" 'long)
  (add-standard-token! "super" 'super)
  (add-standard-token! "while" 'while)
  (add-standard-token! "class" 'class)
  (add-standard-token! "float" 'float)
  (add-standard-token! "native" 'native)
  (add-standard-token! "switch" 'switch)
  (add-standard-token! "const" 'const)
  (add-standard-token! "for" 'for)
  (add-standard-token! "new" 'new)
  (add-standard-token! "synchronized" 'synchronized)
  (add-standard-token! "continue" 'continue)
  (add-standard-token! "goto" 'goto)
  (add-standard-token! "package" 'package)
  (add-standard-token! "this" 'this)

  ;; Lexeme bindings for operators, and other lexemes composed of
  ;; strings of symbols

  (add-standard-token! "(" 'open-paren)
  (add-standard-token! ")" 'close-paren)
  (add-standard-token! "{" 'open-brace)
  (add-standard-token! "}" 'close-brace)
  (add-standard-token! "[" 'open-bracket)
  (add-standard-token! "]" 'close-bracket)
  (add-standard-token! ";" 'semi)
  (add-standard-token! "," 'comma)
  (add-standard-token! "." 'dot)
  (add-standard-token! "=" 'assign)
  (add-standard-token! ">" 'greater)
  (add-standard-token! "<" 'less)
  (add-standard-token! "!" 'bang)
  (add-standard-token! "~" 'tilde)
  (add-standard-token! "?" 'question)
  (add-standard-token! ":" 'colon)
  (add-standard-token! "==" 'equal)
  (add-standard-token! "<=" 'less-or-equal)
  (add-standard-token! ">=" 'greater-or-equal)
  (add-standard-token! "!=" 'not-equal)
  (add-standard-token! "&&" 'logical-and)
  (add-standard-token! "||" 'logical-or)
  (add-standard-token! "++" 'increment)
  (add-standard-token! "--" 'decrement)
  (add-standard-token! "+" 'plus)
  (add-standard-token! "-" 'minus)
  (add-standard-token! "*" 'star)
  (add-standard-token! "/" 'slash)
  (add-standard-token! "&" 'bitwise-and)
  (add-standard-token! "|" 'bitwise-or)
  (add-standard-token! "^" 'bitwise-xor)
  (add-standard-token! "%" 'percent)
  (add-standard-token! "<<" 'shift-left)
  (add-standard-token! ">>" 'shift-right)
  (add-standard-token! ">>>" 'unsigned-shift-right)
  (add-standard-token! "+=" 'plus-assign)
  (add-standard-token! "-=" 'minus-assign)
  (add-standard-token! "*=" 'star-assign)
  (add-standard-token! "/=" 'slash-assign)
  (add-standard-token! "&=" 'and-assign)
  (add-standard-token! "|=" 'or-assign)
  (add-standard-token! "^=" 'xor-assign)
  (add-standard-token! "%=" 'percent-assign)
  (add-standard-token! "<<=" 'shl-assign)
  (add-standard-token! ">>=" 'shr-assign)
  (add-standard-token! ">>>=" 'ushr-assign)

  ;; pseudo bindings so that comment start delimeters arn't treated
  ;; as sequences of two seperate lexemes
  (add-standard-token! "//" 'single-line-comment-begin)
  (add-standard-token! "/*" 'multiple-line-comment-begin)

  ;; lexeme types that are used during execution of the lexer

  ;; string-literal
  ;; character-literal
  ;; float-literal
  ;; integer-literal
  ;; hex-literal
  ;; octal-literal
  ;; identifier
  ;; end-of-file
  )


;;; Code for retrieving a lexeme stream.  These functions make up
;;; the external interface to lex

(define (get-lexeme-stream fstream)
  "Retreve a lexeme stream from an I/O stream"
  (if (port-at-end? fstream)
      ()
      (cons (get-next-token fstream)
            (get-lexeme-stream fstream))))

(define (get-file-lexeme-stream filename)
  "Parse an input file into a lexeme stream"
  (install-standard-tables!)
  (with-port p (open-input-file filename)
    (get-lexeme-stream p)))



