
(in-package :inner-conditional)

;; blah blah blah.
(cl-syntax:use-syntax :annot)

@export
(defmacro with-inner ((&whole args
			      label)
		      &body body)
  @ignore label
  (call-with-inner args body))

(defun call-with-inner (args body)
  (destructuring-bind (label &rest rest) args
    @ignore rest
    (with-gensyms (tag)
      (multiple-value-bind (first conditional-body macrolet-body)
	  (convert-first-inner-to-tag body tag label)
	(if first
	    `(progn ,@body)
	    (precompile-1-layer
	     label
	     (lambda (&rest sexp)
	       (precompile-1-layer
		tag (lambda (&rest args)
		      @ignore args
		      `(progn ,@sexp))
		`(with-inner (,label)
		   ,@macrolet-body)))
	     conditional-body))))))

@eval-always
@export
(defmacro define-inner-conditional
    (name label macro-lambda-list &body body)
  `(progn 
     @eval-always
     (pushnew ',name *precompiling-directives*)
     @eval-always
     (defmacro ,name (,label ,@macro-lambda-list)
       `(inner (,label)
	  ,,@body))))




