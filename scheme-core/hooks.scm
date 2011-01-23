;;;; hooks.scm
;;;;
;;;; The implementation of hooks.

(define (->hook-function hook-fn)
  "Given a hook function name <hook-fn>, return the named hook
   function. A hook function name can either be a symbol or
   or the function itself."
  (cond ((procedure? hook-fn)
         hook-fn)
        ((not (symbol? hook-fn))
         (error "Invalid hook function: ~s" hook-fn))
        ((not (symbol-bound? hook-fn))
         (error "Unbound hook function name: ~s" hook-fn))
        (#t
         (aif (procedure? (symbol-value hook-fn))
              it
              (error "Invalid hook bound to function name: ~s" hook-fn)))))

(define (->hook hook)
  "Given a hook name <hook>, return a list of functions to be invoked when
   the hook is invoked. The hook can be named by either a symbol
   a procedure, or a list of procedures. If named by a symbol, the
   symbol can contain any valid hook name (including another symbol)."
  (cond ((list? hook)
         hook)
        ((procedure? hook)
         (list hook))
        ((not (symbol? hook))
         (error "Invalid hook: ~s" hook))
        ((not (symbol-bound? hook))
         (error "Unbound hook function name: ~s" hook))
        (#t
         (->hook (symbol-value hook)))))



(define (invoke-hook hook . args)
  "Invoke the hook named by the symbol <hook>.  This invokes each
   hook function in the order it was added, passing in <args>. A
   top level dynamic escape named end-hook is established to allow
   hook functions to abort processing prematurely. Returns nothing."
  (check symbol? hook)
  (catch 'end-hook
       (dolist (hook-fn (->hook hook))
               (apply (->hook-function hook-fn) args)))
  (values))

(define (add-hook-function! hook hook-fn)
  "Extend the hook named by the symbol <hook> with the hook function
   <hook-fn>. <hook-fn> can either be a procedure or a symbol naming
   a procedure. It is only added if missing. Returns a list of the
   hook functions currently established on the hook, after the addition."
  (check symbol? hook)
  (let ((hook-functions (->hook hook)))
    (if (memq hook-fn hook-functions)
        hook-functions
        (set-symbol-value! hook (append hook-functions (cons hook-fn))))))

(define (remove-hook-function! hook hook-fn)
  "Removes the hook function named <hook-fn> from the hook named by the
   symbol <hook>. <hook-fn> can either be a procedure or a symbol naming
   a procedure. It is only removed if present.  Returns a list of the
   hook functions currently established on the hook, after the removal."
  (check symbol? hook)
  (set-symbol-value! hook (delete hook-fn (->hook hook) eq?)))

