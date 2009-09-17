(defmacro (doit)
  `(begin
     (eval-when (:load-toplevel) (write 1))
     (eval-when (:compile-toplevel) (write 2))
     (eval-when (:execute) (write 3))))

(doit)

