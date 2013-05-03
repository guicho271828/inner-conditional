

(in-package :inner-conditional)
(cl-syntax:use-syntax :annot)


@eval-always
(export '(inner-when inner-cond inner-if
		  inner-case inner-typecase inner-ccase inner-ecase))

(define-inner-conditional inner-when label (condition &body body)
  `(when ,condition
	 (,label ,@body)))
(define-inner-conditional inner-if label (condition then else)
  `(if ,condition
	   (,label ,then)
	   (,label ,else)))

(define-inner-conditional inner-cond label (&body clauses)
  `(cond
	 ,@(mapcar (lambda (clause)
				 (match clause
				   ((cons condition body)
					`(,condition (,label ,@body)))))
			   clauses)))

(define-inner-conditional inner-case label (keyform &body cases)
  `(case ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))

(define-inner-conditional inner-ecase label (keyform &body cases)
  `(ecase ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))
(define-inner-conditional inner-ccase label (keyform &body cases)
  `(ccase ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))
(define-inner-conditional inner-typecase label (keyform &body cases)
  `(typecase ,keyform
	 ,@(mapcar (lambda (case)
				 (match case
				   ((cons key body)
					`(,key (,label ,@body)))))
			   cases)))
