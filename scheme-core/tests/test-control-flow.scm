(define-package "test-control-flow"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test apply-coverage
  (define (null-result) ())
  (define (atom-result) 1)
  (define (list-result) '(1 2 3 4 5))
  (define (identity xs) xs)
  (define (identity-rest . xs) xs)

  (check (null? (apply null-result '())))
  (check (eq? 1 (apply atom-result '())))
  (check (equal? '(1 2 3 4 5) (apply list-result '())))

  (check (runtime-error? (null? (apply identity '()))))
  (check (null? (apply identity-rest)))
  
  (check (equal? '1 (apply identity '(1))))
  (check (equal? '(1) (apply identity-rest '(1))))
  
  (check (equal? '1 (apply identity '(1 2))))
  (check (equal? '(1 2) (apply identity-rest '(1 2))))

  (check (equal? '(1 2 3) (apply identity-rest 1 '(2 3))))

  (check (equal? '(1 2 3) (apply identity-rest 1 2 '(3))))
  (check (equal? '(1 2 3) (apply identity-rest 1 2 3 '())))

  (check (runtime-error? (apply null-result 2)))
  (check (runtime-error? (apply identity-rest '(1 2 3 4 5 6 7 8 9 10 11 12
                                                  13 14 15 16 17 18 19 20 21
                                                  22 23 24 25 26 27 28 29 30
                                                  31 32 33)))))
  

(define-test do
  (check (equal? (do ((vec (make-vector 5))
                      (i 0 (+ i 1)))
                     ((= i 5) vec)
                   (vector-set! vec i i))
                 [0 1 2 3 4]))
  (check (equal? (let ((x '(1 3 5 7 9)))
                   (do ((x x (cdr x))
                        (sum 0 (+ sum (car x))))
                       ((null? x) sum)))
                 25)))

(define-test throw-catch
  (let ((name-1 (gensym "name"))
        (name-2 (gensym "name")))
    ; Return value of single catch block
    (check (equal? (catch name-1 1) 1))
    (check (equal? (catch name-1 (throw name-1 12) 1) 12))
    
    ; Control flow of single catch block
    (check
     (equal? '(1 2 3)
             (checkpoint-order-of
              (checkpoint 1)
              (catch name-1
                (checkpoint 2)
                (throw name-1 12)
                (checkpoint :does-not-run)
                (throw name-1 24)
                (checkpoint :does-not-run))
              (checkpoint 3))))
   
    ; Return value of double catch block
    (check (equal? (catch name-1 (catch name-2 18)) 18))
    (check (equal? (catch name-1 (catch name-2 (throw name-1 15))) 15))
    (check (equal? (catch name-1 (catch name-2 (throw name-2 15))) 15))
    (check (equal? (catch name-1 (catch name-2 (throw name-2 15)) 21) 21))

    ; Control flow of double catch block
    (check
     (equal? '(1 2 3 4)
             (checkpoint-order-of
              (checkpoint 1)
              (catch name-1
                (checkpoint 2)
                (catch name-2
                  (checkpoint 3)
                  (throw name-1)
                  (checkpoint :does-not-run))
                (checkpoint :does-not-run))
              (checkpoint 4))))

    ; Catch with dynamic extent
    (letrec ((execute-with-catch (lambda (name fn) (catch name (fn))))
	     (execute-throw (lambda (name rc) (throw name rc))))
      
      (check (equal? (execute-with-catch name-1 (lambda () 12)) 12))
      (check (equal? (execute-with-catch name-1 (lambda () (throw name-1 12) 24)) 12))
      (check (equal? (catch name-1 (execute-throw name-1 12) 24) 12))
      
      (check
       (equal? '(1 2 3 4)
               (checkpoint-order-of
                (checkpoint 1)
                (catch name-1
                  (checkpoint 2)
                  (execute-with-catch name-1 (lambda () 
                                               (checkpoint 3)
                                               (throw name-1)
                                               (checkpoint :does-not-run)))
                  ;; Lexically scoped catch/throw would skip this step
                  (checkpoint 4))))))))

(define-test typecase
  (check (runtime-error? (macroexpand '(typecase x 1 ((foo) bar)))))
  (check (runtime-error? (macroexpand '(typecase x ((foo 12) bar)))))
  (check (runtime-error? (macroexpand '(typecase x (foo bar)))))
  (check (runtime-error? (macroexpand '(typecase x (12 bar)))))

  (check (equal? :foo (typecase 1
                            ((fixnum) :foo)
                            ((flonum) :xyzzy))))

  (check (not (runtime-error? (typecase 'symbol
                                    ((fixnum) 12)))))
  

  (check (equal? :xyzzy (typecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((flonum symbol) :xyzzy))))

  (check (equal? :xyzzy (typecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((symbol flonum) :xyzzy))))

  (check (equal? 12 (typecase 'symbol
                          ((fixnum) :foo)
                          ((character) :bar)
                          ((flonum) :xyzzy)
                          (#t 12))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (typecase #\a
              ((fixnum) 12)
              ((character)
               (checkpoint 1)
               (checkpoint 2)
               (checkpoint 3)))))))

(define-test etypecase
  (check (runtime-error? (macroexpand '(etypecase x 1 ((foo) bar)))))
  (check (runtime-error? (macroexpand '(etypecase x ((foo 12) bar)))))
  (check (runtime-error? (macroexpand '(etypecase x (foo bar)))))
  (check (runtime-error? (macroexpand '(etypecase x (12 bar)))))

  (check (runtime-error? (macroexpand '(etypecase x ((fixnum) bar) (#t baz)))))

  (check (equal? :foo (etypecase 1
                            ((fixnum) :foo)
                            ((flonum) :xyzzy))))

  (check (runtime-error? (etypecase 'symbol
                               ((fixnum) :foo)
                               ((character) :bar)
                               ((flonum) :xyzzy))))

  (check (equal? :xyzzy (etypecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((flonum symbol) :xyzzy))))

  (check (equal? :xyzzy (etypecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((symbol flonum) :xyzzy))))
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (etypecase #\a
              ((fixnum) 12)
              ((character)
               (checkpoint 1)
               (checkpoint 2)
               (checkpoint 3)))))))

(define-test unwind-protect-coverage
  ;; No error
  (let ((after-evaluated? #f))
    (unwind-protect
     (lambda ())
     (lambda () (set! after-evaluated? #t)))
    (check after-evaluated?))
  
  ;; No error - return value
  (let ((after-evaluated? #f))
    (check (eq? 'foo
                    (unwind-protect
                     (lambda () 'foo)
                     (lambda () (set! after-evaluated? #t)))))
    (check after-evaluated?))

  ;; No error - return value (w/GC)
  (let ((after-evaluated? #f))
    (check (eq? 'foo
                    (unwind-protect
                     (lambda () (gc) 'foo)
                     (lambda () (set! after-evaluated? #t)))))
    (check after-evaluated?))

  ;; Error
  (let ((after-evaluated? #f))
    (catch-all
     (unwind-protect
      (lambda () (throw 'frobozzle))
      (lambda () (set! after-evaluated? #t))))
    (check after-evaluated?))

  ;; Nested - No Error
  (let ((sequence-number 0)
        (after-evaluated-1? #f)
        (after-evaluated-2? #f)
        (after-evaluated-3? #f))

    (catch-all
     (unwind-protect
      (lambda ()
        (unwind-protect
         (lambda ()
           (unwind-protect (lambda ())
                           (lambda () 
                             (incr! sequence-number)
                             (set! after-evaluated-1? sequence-number))))
         (lambda () 
           (incr! sequence-number)
           (set! after-evaluated-2? sequence-number))))
      (lambda () 
        (incr! sequence-number)
        (set! after-evaluated-3? sequence-number))))

    (check (eq? after-evaluated-1? 1))
    (check (eq? after-evaluated-2? 2))
    (check (eq? after-evaluated-3? 3)))
  
  ;; Nested - Error
  (let ((sequence-number 0)
        (after-evaluated-1? #f)
        (after-evaluated-2? #f)
        (after-evaluated-3? #f))

    (catch-all
     (unwind-protect
      (lambda ()
        (unwind-protect
         (lambda ()
           (unwind-protect
            (lambda () 
              (set! sequence-number 10)
              (throw 'foo))
            (lambda () 
              (incr! sequence-number)
              (set! after-evaluated-1? sequence-number))))
         (lambda () 
           (incr! sequence-number)
           (set! after-evaluated-2? sequence-number))))
      (lambda () 
        (incr! sequence-number)
        (set! after-evaluated-3? sequence-number))))
    (check (eq? after-evaluated-1? 11))
    (check (eq? after-evaluated-2? 12))
    (check (eq? after-evaluated-3? 13)))

  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (let ((return-value (begin
                                  (let ((catch-return-value (catch 'test-name
                                                              (checkpoint 1)
                                                              (unwind-protect (lambda () 
                                                                                (checkpoint 2))
                                                                              (lambda () 
                                                                                (checkpoint 3)
                                                                                (throw 'test-name :test-return-value)
                                                                                (checkpoint :unreached-1)))
                                                              (checkpoint :unreached-2))))
                                    (checkpoint 4 catch-return-value)))))
              (check (equal? :test-return-value return-value)))))))

(defmacro (test-macro-1 a b c)
  (list a b c))

(defmacro (test-macro-2 . x)
  x)

(define-test macro-expansion
  (check
   (equal? '(foo bar baz) (macroexpand '(test-macro-1 foo bar baz))))
  (check 
   (equal? '(foo bar baz) (macroexpand '(test-macro-2 foo bar baz)))))
