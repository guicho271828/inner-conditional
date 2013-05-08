
(in-package :inner-conditional)

;; blah blah blah.
(cl-syntax:use-syntax :annot)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; expansion hook / continuation
;; (defvar *previous-hooks* nil)
;; (defun reset-hooks ()
;;   (setq *previous-hooks* nil
;; 	*macroexpand-hook* 'funcall))
;; (defun push-hook (hook)
;;   (push *macroexpand-hook* *previous-hooks*)
;;   (setf *macroexpand-hook* hook))
;; (defun pop-hook ()
;;   (setf *macroexpand-hook* (pop *previous-hooks*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inner clause
(define-condition compile-time-condition (simple-condition)
  ((form :initarg :form :accessor form)
   (environment :initarg :environment :accessor environment)))

(defmethod print-object ((c compile-time-condition) s)
  (print-unreadable-object (c s)
    (with-slots (form) c
      (format s "~a" form))))

(define-condition inner-condition (compile-time-condition)
  ((label :initarg :label :accessor label)
   (tag :initarg :tag :accessor tag)
   (versions :initarg :versions :accessor versions :initform nil)
   (body :initarg :body :accessor body)))

(defmethod print-object ((c inner-condition) s)
  (print-unreadable-object (c s)
    (with-slots (label tag versions) c
      (format s "INNER ~a ~a ~a" label tag versions))))

@export
(defmacro inner (&whole form (label &key originally) &body body)
  (restart-case
      (with-gensyms (tag)
	(error 'inner-condition
		:tag tag
		:label label
		:form (or originally form)
		:body body))
    (use-value (value)
      :test (lambda (c) (eq label (label c)))
      value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-inner

(defun try-compilation (form)
  (handler-bind 
      ((warning
	(lambda (c)
	  (muffle-warning c))))
    (compile (gensym) `(lambda () ,form))))

@export
(defmacro with-inner ((label) &body body)
  (call-with-inner label body))

(defun call-with-inner (label body)
  (let (inners)
    ;; 1st pass :: search the body for inner
    (handler-bind
	(;(simple-error (lambda (c) (invoke-debugger c)))
	 (inner-condition
	  (lambda (c)
	    ;; (format t "handling ~a target: ~a condition: ~a~%"
	    ;; 	    (if (eq label (label c))
	    ;; 		:success :fail) label c)
	    (if (eq label (label c))
		(progn
		  (push c inners)
		  (setf body
			(subst-1 (tag c) (form c) body :test #'equalp))
		  (use-value '(progn) c))
		(setf body
		      (subst-1 '(progn) (form c) body :test #'equalp))))))
      (try-compilation `(progn ,@body)))
    (setf inners (nreverse inners))
    ;; (format t "Pass 1 finished: label: ~a inners: ~a~%" label inners)
    ;; 2nd pass :: process inside inner
    (mapc
     (lambda (inner)
       (handler-bind
	   ((version-condition
	     (lambda (c)
	       (if (and (eq (label inner) (label c))
			(eq (tag inner) (tag c)))
		   (progn
		     (push c (versions inner))
		     (setf (body inner)
			   (subst-1 (version-tag c) (form c)
				    (body inner) :test #'equalp)))
		   (setf body
			 (subst-1 '(progn) (form c)
				  body :test #'equalp)))
	       (use-value '(progn) c))))
	 (try-compilation
	  (wrap-with-body-macrolet inner))))
     inners)
    ;; (format t "Pass 2 finished: label: ~a inners: ~a~%" label inners)
    ;; final pass :: form a final expansion
    (form-expansion inners body)))

(define-condition version-condition (compile-time-condition)
  ((label :initarg :label :accessor label)
   (tag :initarg :tag :accessor tag)
   (version-tag :initarg :version-tag :accessor version-tag)
   (body :initarg :body :accessor body)))

(defmethod print-object ((c version-condition) s)
  (print-unreadable-object (c s)
    (with-slots (label tag version-tag) c
      (format s "VERSION ~a ~a ~a" label tag version-tag))))

(defun wrap-with-body-macrolet (inner)
  `(macrolet
       ((,(label inner) (&whole form &body body)
	  (with-gensyms (,(tag inner))
	    (restart-case
		(error 'version-condition
		       :tag ',(tag inner)
		       :version-tag ,(tag inner)
		       :label ',(label inner)
		       :form form
		       :body body)
	      (use-value (value)
		:test (lambda (c) 
			(and (eq ',(label inner) (label c))
			     (eq ,(tag inner) (version-tag c))))
		value)))))
     ,@(body inner)))

(defun form-expansion (inners main-body)
  (if (not inners)
      `(progn ,@main-body)
      (let ((inner (car inners)))
	`(symbol-macrolet 
	     ,(mapcar
	       (lambda (version)
		 `(,(version-tag version)
		    (symbol-macrolet
			((,(tag inner) (progn ,@(body version))))
		      ,(form-expansion (cdr inners) main-body))))
	       (versions inner))
	   ,@(body inner)))))

@eval-always
@export
(defmacro define-inner-conditional
    (name label macro-lambda-list &body body)
  (with-gensyms (form)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,name (&whole ,form ,label ,@macro-lambda-list)
	 `(inner (,label :originally ,,form)
	    ,,@body)))))




