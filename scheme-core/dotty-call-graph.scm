
(define-package "dotty-call-graph"
  (:uses "scheme"
         "call-graph")
  (:exports "dotty-call-graph"))


(define (write-dotty-call-graph cg :optional (op (current-output-port)))
  (define (format-name name)
    (format #f "\"~a::~a\"" (package-name (symbol-package name)) (symbol-name name)))
  (format op "digraph G {\n")
  (dolist (caller-info cg)
    (let ((caller (car caller-info)))
      (dolist (callee (cdr caller-info))
        (format op "   ~a->~a;\n" (format-name caller) (format-name callee))))
    (format op "\n"))
  (format op "}\n"))

;; (define (package-call-graph package)
;;   (let ((cg (call-graph::package-call-graph package)))
;;     ))

