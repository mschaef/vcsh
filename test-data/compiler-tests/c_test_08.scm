
(defmacro (defbench benchname . code)
  `(set!  ,benchname (lambda () ,@code x)))

(defbench foo (set! x 12))