(rename-package! "fasl-compiler" "compiler")

(defalias compiler::compile-toplevel-form compiler::compile-form)