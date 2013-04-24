#|
  This file is a part of inner-conditional project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inner-conditional
  (:use :cl :optima :alexandria))
(in-package :inner-conditional)

;; blah blah blah.
(cl-syntax:use-syntax :annot)

(defpattern when (condition body)
  `(list* 'when ,condition ,body))

(defpattern if (condition then else)
  `(list 'if ,condition ,then ,else))

(defpattern cond (clauses)
  `(list 'cond
		 ,@(mapcar (lambda (clause)
					 `(list* ,@clause)) clauses)))

@export
(defmacro with-inner ((&whole args label) &body body)
  (call-with-inner args body))

(defpattern inner (label body)
  `(list 'inner (list (eq ,label))
		 ,body))

(defun match-inner (label elem)
  (match elem
	((inner label (when cond body))
	 (values t `(if ,cond
					,@body
					(,label nil))))
	((inner label body)
	 (values t body))))
  

@export
(defmacro inner ((label) &body body)
  @ignore label body)

(defun call-with-inner (args body)
  (destructuring-bind (label) args
	(with-gensyms (conditional-tag)
	  (let ((conditional-body nil)
			(first t))
		(let ((new-body
			   (subst-if
				conditional-tag
				#'(lambda (elem)
					(and first
						 (multiple-value-bind (flag body)
							 (match-inner label elem)
						   (when flag
							 (setf conditional-body body
								   first nil))
						   flag)))
				body)))
		  (if first
			  `(progn ,@body)
			  `(macrolet ((,label (&rest sexp)
							(subst `(progn ,@sexp) ',conditional-tag
								   '(with-inner (,label)
									 ,@new-body))))
				 ,conditional-body)))))))

;; no cool at all. should be ommited?
;; or should be still here for extension?

@export
(defmacro define-inner-conditional (name label macro-lambda-list body)
  `(defmacro ,name (,label ,@macro-lambda-list)
	 `(inner (,label)
		,,body)))

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

