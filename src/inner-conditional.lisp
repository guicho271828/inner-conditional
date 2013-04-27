#|
  This file is a part of inner-conditional project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inner-conditional
  (:use :cl :optima :iterate :annot.doc :annot.eval-when :alexandria))
(in-package :inner-conditional)

;; blah blah blah.
(cl-syntax:use-syntax :annot)

@export
(defmacro with-inner ((&whole args
							  label
					   &key   force-single-check
					          current-version) &body body)
  @ignore label current-version
  (if force-single-check
	  (call-with-single-condition-inner args body)
	  (call-with-inner args body)))

@eval-always
@export
(defvar *precompiling-directives* nil)

(defun walk-tree (fn tree)
  (mapcar (lambda (branch)
			(funcall fn branch
					 (lambda (branch)
					   (walk-tree fn branch))))
		  tree))

(defun precompile-directives (form)
  (walk-tree
   (lambda (subform cont)
	 (iter (with expanded = subform)
		   (when (member (typecase expanded
						   (cons (car expanded))
						   (symbol expanded))
						 *precompiling-directives*)
			 (setf expanded (macroexpand-1 subform))
			 (next-iteration))
		   (return (if (consp expanded)
					   (funcall cont expanded)
					   expanded))))
   form))

(defun precompile-directives-1 (form)
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
						 expanded (macroexpand-1 subform))
				   (next-iteration))
				 (return (if (consp expanded)
							 (funcall cont expanded)
							 expanded)))
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
		  (,label nil)))
	((inner label body) body)))

(defun convert-first-inner-to-tag (body tag label)
  (let ((first t) conditional-body macrolet-body)
	(setf macrolet-body
		  (subst-if
		   tag
		   #'(lambda (elem)
			   (when first
				 (let ((body (match-inner label elem)))
				   (when body
					 (setf first nil conditional-body body)
					 t))))
		   (precompile-directives body)))
	(values first conditional-body macrolet-body)))

(defun call-with-inner (args body)
  (destructuring-bind (label &rest rest) args
	@ignore rest
	(with-gensyms (tag)
	  (multiple-value-bind (first conditional-body macrolet-body)
		  (convert-first-inner-to-tag body tag label)
		(if first
			`(progn ,@body)
			`(macrolet ((,label (&rest sexp)
						  `(symbol-macrolet ((,',tag `(progn ,',@sexp)))
							 (with-inner (,',label)
							   ,@',macrolet-body))))
			   ,conditional-body))))))

(defun convert-first-conditional (body tag label)
  (let ((first t) conditional-body macrolet-body)
	(setf macrolet-body
		  (subst-if
		   tag
		   #'(lambda (elem)
			   (when first
				 (let ((body (match-inner label elem)))
				   (when body
					 (setf first nil conditional-body body)
					 t))))
		   (precompile-directives-1 body)))
	(values first conditional-body macrolet-body)))

(defun call-with-single-condition-inner (args body)
  (destructuring-bind (label &key
							 force-single-check
							 current-version) args
	(setf *current-version* current-version)
	(with-gensyms (tag)
	  (multiple-value-bind (first conditional-body macrolet-body)
		  (convert-first-conditional body tag label)
		(if first
			`(progn ,@body)
			`(macrolet ((,label (id &rest sexp)
						  `(symbol-macrolet ((,',tag `(progn ,',@sexp)))
							 (with-inner (,',label
										  :force-single-check
										  ,',force-single-check
										  :current-version
										  ,id)
							   ,@',macrolet-body))))
			   ,conditional-body))))))

@eval-always
@export
(defparameter *current-version* nil)

@export
(defmacro with-versions (bindings &body body)
  ;;debug
  (format t "~%current version is ~a" *current-version*)
  (if *current-version*
	  `(symbol-macrolet ,bindings
		 ,@body)
	  `(,*current-version*)))


;; no cool at all. should be ommited?
;; or should be still here for extension?

@export
(defmacro define-inner-conditional
	(name label macro-lambda-list &body body)
  (pushnew name *precompiling-directives*)
  `(defmacro ,name (,label ,@macro-lambda-list)
	 `(inner (,label)
		,,@body)))

@export
(define-inner-conditional inner-when label (condition &body body)
  `(when ,condition
	 (,label ,@body)))

@export
(define-inner-conditional inner-if label (condition then else)
  `(if ,condition
	   (,label ,then)
	   (,label ,else)))

@export
(define-inner-conditional inner-cond label (&body clauses)
  `(cond
	 ,@(mapcar (lambda (clause)
				 (match clause
				   ((cons condition body)
					`(,condition (,label ,@body)))))
			   clauses)))


@export
(define-inner-conditional inner-case label (keyform &body cases)
  `(case ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))

@export
(define-inner-conditional inner-ecase label (keyform &body cases)
  `(ecase ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))
@export
(define-inner-conditional inner-ccase label (keyform &body cases)
  `(ccase ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))
@export
(define-inner-conditional inner-typecase label (keyform &body cases)
  `(typecase ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))



@export
(defmacro define-condition-expander
	((name expander-id expander-name
		   &key force-single-check version-expander)
	 macro-lambda-list &body body)
  (pushnew name *precompiling-directives*)
  (with-gensyms (versions)
	`(progn
	   (defmacro ,expander-name (&body body)
		 `(with-inner (,,expander-id 
					   :force-single-check ,,force-single-check)
			,@body))
	   
	   (defmacro ,name ,macro-lambda-list
		 `(inner (,,expander-id)
			,(let ((,versions nil))
				  (flet ((,version-expander (id body)
						   (push (list id 
									   `(,,expander-id ,id ,body))
								 ,versions)
						   id))
					(let ((condition-body ,@body))
					  `(with-versions ,,versions
						 ,condition-body)))))))))