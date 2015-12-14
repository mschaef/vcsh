(use-package! "unit-test")

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
