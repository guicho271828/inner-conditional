;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for define-condition-expander
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :inner-conditional)
(cl-syntax:use-syntax :annot)

(defun %macro-lambda-list-whole-body (lambda-list)
  (let* ((environment
	  (if-let ((sub (member '&environment lambda-list)))
	    (second sub)
	    (with-gensyms (environment)
	      (push environment lambda-list)
	      (push '&environment lambda-list)
	      environment)))
	 (whole
	  (if-let ((sub (member '&whole lambda-list)))
	    (second sub)
	    (with-gensyms (whole)
	      (push whole lambda-list)
	      (push '&whole lambda-list)
	      whole))))
    (list lambda-list environment whole)))

@eval-always
@export
(defmacro define-condition-expander
    ((inner-name outer-name &optional (version-expander inner-name))
     lambda-list &body body)
  (with-gensyms (%condition)
    (destructuring-bind (lambda-list environment form)
	(%macro-lambda-list-whole-body lambda-list)
      `(progn
	 (define-condition ,%condition (inner-condition)
	   ())
	 (defmacro ,inner-name ,lambda-list
	   (with-gensyms (,inner-name)
	     (signal ',%condition
		     :tag ,inner-name
		     :label ',inner-name
		     :env ,environment
		     :form ,form
		     :body `(,,@body))
	     ,@body))
	 (defmacro ,version-expander (&body body)
	   `(progn ,@body))
	 (defmacro ,outer-name (&body body &environment env)
	   (call-with-inner
	    ',version-expander body
	    (curry #'call-with-inner-pass1 ',inner-name)
	    (rcurry #'call-with-inner-pass2 ',version-expander)
	    #'form-expansion-with-single-condition
	    env))))))

(defun form-expansion-with-single-condition (inners outer-tag)
  (let* ((inner (car inners)))
    `(symbol-macrolet 
	 ,(iter
	   (for version in (versions inner))
	   (for i from 0)
	   (collecting
	    `(,(version-tag version)
	       (symbol-macrolet
		   ,(mapcar
		     (lambda (inner)
		       `(,(tag inner)
			  (progn
			    ,@(body (nth i (versions inner))))))
		     inners)
		 ,outer-tag))))
       ,(body inner))))

