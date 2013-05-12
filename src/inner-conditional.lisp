
(in-package :inner-conditional)

;; blah blah blah.
(cl-syntax:use-syntax :annot)

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
(defmacro inner (&whole form (label) &body body &environment env)
  ;; @ignorable env 
  (with-gensyms (inner)
    (error 'inner-condition
	   :environment env
	   :tag inner
	   :label label
	   :form form
	   :body body)))

(defun use-value-hook (prev)
  (lambda (expander form env)
    (restart-case
	(funcall prev expander form env)
      (use-value (value)
	value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-inner

(defun call-with-inner-pass1 (label expand)
  (let* ((inners nil)
	 (expansion 
	  (handler-bind
	      ((inner-condition
		(lambda (c)
		  ;; (format t "handling ~a. target: ~a condition: ~a~%"
		  ;; 	  (if (eq label (label c))
		  ;; 	      :success :fail) label c)
		  (when (eq label (label c))
		    ;; (print (environment c))
		    (push c inners)
		    (use-value (tag c) c)))))
	    (funcall expand))))
    (values expansion (nreverse inners))))

(defun call-with-inner-pass2 (inner expand
			       &optional
			       (label (label inner)))
  (handler-bind
      ((version-condition
	(lambda (c)
	  (when (and (eq label (label c))
		     (eq (tag inner) (tag c)))
	    (push c (versions inner))
	    (setf (body c)
		  (macroexpand-dammit
		   (macroexpand-dammit
		    (body c)
		    (environment c))
		   (environment inner)))
	    (use-value (version-tag c) c)))))
    (funcall expand)))

(defun call-with-inner (label body
			outer-handler
			inner-body-handler
			final-expander
			&optional env)
  (with-hook (#'use-value-hook)
    ;; 1st pass :: search the body for inner
    (multiple-value-bind
	  (outer-expansion inners)
	(funcall outer-handler
		 (lambda ()
		     (macroexpand-dammit
		      `(progn ,@body) env)))
      
      ;; (format t "Pass 1 finished: label: ~a inners: ~a~%" label inners)
      ;; 2nd pass :: process inside inner
      (mapc
       (lambda (inner)
	 (setf (body inner)
		(funcall
		 inner-body-handler
		 inner
		 (lambda ()
		   (macroexpand-dammit
		    (wrap-with-body-macrolet label inner)
		    (environment inner))))))
       inners)
      ;; (format t "Pass 2 finished: label: ~a inners: ~a~%" label inners)
      ;; final pass :: form a final expansion
      (with-gensyms (outer-tag)
	`(symbol-macrolet ((,outer-tag ,outer-expansion))
	   ,(funcall final-expander inners outer-tag))))))

(define-condition version-condition (compile-time-condition)
  ((label :initarg :label :accessor label)
   (tag :initarg :tag :accessor tag)
   (version-tag :initarg :version-tag :accessor version-tag)
   (body :initarg :body :accessor body)))

(defmethod print-object ((c version-condition) s)
  (print-unreadable-object (c s)
    (with-slots (label tag version-tag) c
      (format s "VERSION ~a ~a ~a" label tag version-tag))))

(defun wrap-with-body-macrolet (label inner)
  `(macrolet
       ((,label (&whole form &body body &environment env)
	  (with-gensyms (inner-body)
	    (error 'version-condition
		   :environment env
		   :tag ',(tag inner)
		   :version-tag inner-body
		   :label ',label
		   :form form
		   :body body))))
     ,@(body inner)))

(defun form-expansion (inners outer-tag)
  (if (not inners)
      outer-tag
      (let ((inner (car inners)))
	`(symbol-macrolet 
	     ,(mapcar
	       (lambda (version)
		 ;; (print (environment version))
		 `(,(version-tag version)
		    (symbol-macrolet
			((,(tag inner)
			  (progn ,@(body version))))
		      ,(form-expansion (cdr inners) outer-tag))))
	       (versions inner))
	   ,(body inner)))))

@export
(defmacro with-inner ((label) &body body &environment env)
  (call-with-inner
   label body
   (curry #'call-with-inner-pass1 label)
   #'call-with-inner-pass2
   #'form-expansion env))

@eval-always
@export
(defmacro define-inner-conditional
    (name label macro-lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmacro ,name (,label ,@macro-lambda-list)
       `(inner (,label)
	  ,,@body))))




