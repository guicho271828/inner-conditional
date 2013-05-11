
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
(defmacro inner (&whole form (label &key originally) &body body)
  (with-gensyms (inner)
    (error 'inner-condition
	   :tag inner
	   :label label
	   :form (or originally form)
	   :body body)))

(defun use-value-hook (prev)
  (lambda (expander form env)
    (restart-case
	(funcall prev expander form env)
      (use-value (value)
	value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-inner

@export
(defmacro with-inner ((label) &body body &environment env)
  (call-with-inner label body env))

(defun call-with-inner (label body &optional env)
  (with-hook (#'use-value-hook)
    (let ((inners nil))
      ;; 1st pass :: search the body for inner
      (let ((outer-expansion
	     (handler-bind
		 ((inner-condition
		   (lambda (c)
		     (format t "~%handling ~a. target: ~a condition: ~a"
		     	    (if (eq label (label c))
		     	    	:success :fail) label c)
		     (when (eq label (label c))
		       (push c inners)
		       (use-value (tag c) c)))))
	       (macroexpand-dammit `(progn ,@body) env))))
	(setf inners (nreverse inners))
	(format t "~%Pass 1 finished: label: ~a inners: ~a" label inners)
	;; 2nd pass :: process inside inner
	(mapc
	 (lambda (inner)
	   (setf (body inner)
		 (handler-bind
		     ((version-condition
		       (lambda (c)
			 (when (and (eq (label inner) (label c))
				    (eq (tag inner) (tag c)))
			   (push c (versions inner))
			   (use-value (version-tag c) c)))))
		   (macroexpand-dammit
		    (wrap-with-body-macrolet inner) env))))
	 inners)
	(format t "~%Pass 2 finished: label: ~a inners: ~a" label inners)
	;; final pass :: form a final expansion
	(with-gensyms (outer-tag)
	  ;; `(symbol-macrolet ((,outer-tag ,outer-expansion))
	  ;;    ,(form-expansion inners outer-tag))
	  (let ((result `(symbol-macrolet ((,outer-tag ,outer-expansion))
			   ,(form-expansion inners outer-tag))))
	    (format t "~%expansion result :~
                       ~%~a
                       ~%
                       ~%________________________________"
		     result)
	    result))))))

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
	  (with-gensyms (inner-body)
	    (error 'version-condition
		   :tag ',(tag inner)
		   :version-tag inner-body
		   :label ',(label inner)
		   :form form
		   :body body))))
     ,@(body inner)))

;; (use-value (value)
;; 	   :test (lambda (c) 
;; 		   (and (eq ',(label inner) (label c))
;; 			(eq ,(tag inner) (version-tag c))))
;; 	   value)

(defun form-expansion (inners outer-tag)
  (if (not inners)
      outer-tag
      (let ((inner (car inners)))
	`(symbol-macrolet 
	     ,(mapcar
	       (lambda (version)
		 `(,(version-tag version)
		    (symbol-macrolet
			((,(tag inner) (progn ,@(body version))))
		      ,(form-expansion (cdr inners) outer-tag))))
	       (versions inner))
	   ,(body inner)))))

@eval-always
@export
(defmacro define-inner-conditional
    (name label macro-lambda-list &body body)
  (with-gensyms (form)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,name (&whole ,form ,label ,@macro-lambda-list)
	 `(inner (,label :originally ,,form)
	    ,,@body)))))




