(define-package "utilities"
  (:uses "scheme")
  (:exports "random-case"))


(defmacro (random-case . cases)
  "Randomly evaluates one of its cases, with the specified probabilities.
   Each case takes the form ( <probability> . <code> ) and is evaluated
   with the probability <probability> / (sum <all probabilities>). "

  ;;  Validate the code being expanded
  (let loop ((remaining cases))
    (unless (null? remaining)
      (let ((current-case (car remaining)))
        (unless (list? current-case)
          (error "random-case cases must be lists" current-case))
        (unless (number? (car current-case))
          (error "random-case case probabilities must be numbers" (car current-case)))
        (unless (>= (car current-case) 0)
          (error "random-case case probabilities must be non-negative" (car current-case)))
        (loop (cdr remaining)))))

  ;; Expand it
  (let ((total-probabilities (apply + (map car cases)))
        (cumulative-probability-so-far 0))
    (with-gensyms (random-value-sym)
      `(let ((,random-value-sym (random ,total-probabilities)))
         (cond
          ,@(map (lambda (current-case)
                   (incr! cumulative-probability-so-far (car current-case))
                   `((< ,random-value-sym ,cumulative-probability-so-far) ,@(cdr current-case)))
                 cases))))))
