
(in-package :inner-conditional)
(cl-syntax:use-syntax :annot)

(defun walk-tree (fn tree)
  (funcall fn tree
	   (lambda (branch)
	     (mapcar (lambda (branch)
		       (walk-tree fn branch))
		     branch))))


@eval-always
@export
(defvar *precompiling-directives* nil)

(defun precompile-directives (form &optional env)
  (walk-tree
   (lambda (subform cont)
     (iter (with expanded = subform)
	   (when (member (typecase expanded
			   (cons (car expanded))
			   (symbol expanded))
			 *precompiling-directives*)
	     (setf expanded (macroexpand-1 expanded env))
	     (next-iteration))
	   (return (if (consp expanded)
		       (funcall cont expanded)
		       expanded))))
   form))

(defun precompile-directives-1 (form &optional env)
  (walk-tree
   (let ((first t))
     (lambda (subform cont)
       (if first
	   (iter (with expanded = subform)
		 (when (member (typecase expanded
				 (cons (car expanded))
				 (symbol expanded))
			       *precompiling-directives*)
		   (setf first nil
			 expanded (macroexpand-1 expanded env))
		   (next-iteration))
		 (return (if (consp expanded)
			     (funcall cont expanded)
			     expanded)))
	   subform)))
   form))

(defun precompile-1-layer (sym fn form)
  (walk-tree
   (lambda (subform cont)
     (if (and (consp subform)
	      (equalp sym (car subform)))
	 (apply fn (cdr subform))
	 (if (consp subform)
	     (funcall cont subform)
	     subform)))
   form))


@export
@doc "Defined in order to provide the editor support.
This code will be expanded only when no =with-inner=
is wrapping it. It just ignores =label= and put the body
where it is originally located."
(defmacro inner ((label) &body body)
  `(macrolet ((,label (&rest sexp)
		`(progn ,@sexp)))
     ,@body))

(defpattern when (condition body)
  `(list* 'when ,condition ,body))
(defpattern if (condition then else)
  `(list 'if ,condition ,then ,else))
(defpattern cond (clauses)
  `(list 'cond
	 ,@(mapcar (lambda (clause)
		     `(list* ,@clause)) clauses)))

(defpattern inner (label body)
  `(list 'inner (list (eq ,label))
	 ,body))

@doc "check whether =elem= is a =inner= clause.
convert =when= if necessary."
(defun match-inner (label elem)
  (match elem
    ((inner label (when cond body))
     `(if ,cond
	  ,@body
	  (,label)))
    ((inner label body) body)))

(defun convert-first-inner-to-tag (body tag label)
  (let ((first t) conditional-body macrolet-body)
    (setf macrolet-body
	  (subst-if
	   `(,tag)
	   #'(lambda (elem)
	       (when first
		 (let ((body (match-inner label elem)))
		   (when body
		     (setf first nil conditional-body body)
		     t))))
	   body))
    (values first conditional-body macrolet-body)))