
(set! *uncompiled-function-handler*
      (lambda (function)
	(error "Uncompiled function [ ~a ]" function)))
