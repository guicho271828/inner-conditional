
(in-package :inner-conditional)

;; blah blah blah.
(cl-syntax:use-syntax :annot)

(define-condition inner (simple-condition)
  ((label :initarg :label :accessor label)
   (tag :initarg :tag :accessor tag)
   (body :initarg :body :accessor body)
   (form :initarg :form :accessor form)))


@export
(defmacro inner (&whole form (label) &body body)
  (restart-case
      (with-gensyms (tag)
	(signal 'inner
		:tag tag
		:label label
		:form form
		:body body))
    (use-tag (c)
      :test (lambda (c) (eq label (label c)))
      (print :invoked)
      (tag c))))

(defvar *previous-hooks* nil)

(handler-case
    (compile nil
	     '(lambda ()
	       (print a)))
  (warning (w)
    (print w)))


(defun reset-hooks ()
  (setq *previous-hooks* nil
	*macroexpand-hook* 'funcall))

@export
(defmacro with-inner ((label) &body body &environment env)
  (%call-with-inner label body)
  `(progn
     ,@body
     (with-inner-end)))

(defun push-hook (hook)
  (push *macroexpand-hook* *previous-hooks*)
  (setf *macroexpand-hook* hook))
(defun pop-hook ()
  (setf *macroexpand-hook* (pop *previous-hooks*)))

(defun %call-with-inner (label body)
  (let ((prev *macroexpand-hook*))
    (push prev *previous-hooks*)
    (setf *macroexpand-hook*
	  (%call-with-inner-hook prev label body))))

(defun %call-with-inner-hook (prev label body)
  (lambda (expander form env)
    (let (inners)
      (let ((tagged-body
	     (handler-bind
		 ((inner (lambda (c)
			   (when (eq (label c) label)
			     (invoke-restart
			      (find-restart 'use-tag c)
			      c)))))
	       (funcall prev expander form env))))
	tagged-body
	;; (render-templates tagged-body inners)
	))))

;; (defun adjust-body (form c)
;;   (with-gensyms (tag)
;;     `(symbol-macrolet ((,tag ,(body c)))
;;        ,(subst-1 tag (form c) form :test #'equalp))))

;; (defun subst-1 (new old tree &key (test #'eql))
;;   (let ((first t))
;;     (subst
;;      new old tree :test
;;      (lambda (e1 e2)
;;        (let ((result (and first (funcall test e1 e2))))
;; 	 (when result
;; 	   (setf first nil)
;; 	   result))))))

;; (subst-1 2 1 '(8 8 6 (4 7 1 6) 1 9 3))

;; (defun inner-handler (label main-body)
;;   (lambda (c)
;;     (when (eq (label c) label)
;;       (with-gensyms (tag)
;; 	(setf (tag c) tag)
;; 	(invoke-restart 'become-a-tag c)
;; 	(invoke-restart 'wrap-and-expand-again c)))))
      
    



;; (defun call-with-inner (args body)
;;   (destructuring-bind (label &rest rest) args
;;     @ignore rest
;;     (with-gensyms (tag)
;;       (multiple-value-bind (first conditional-body macrolet-body)
;; 	  (convert-first-inner-to-tag body tag label)
;; 	(if first
;; 	    `(progn ,@body)
;; 	    (precompile-1-layer
;; 	     label
;; 	     (lambda (&rest sexp)
;; 	       (precompile-1-layer
;; 		tag (lambda (&rest args)
;; 		      @ignore args
;; 		      `(progn ,@sexp))
;; 		`(with-inner (,label)
;; 		   ,@macrolet-body)))
;; 	     conditional-body))))))

;; @eval-always
;; @export
;; (defmacro define-inner-conditional
;;     (name label macro-lambda-list &body body)
;;   `(progn 
;;      @eval-always
;;      (pushnew ',name *precompiling-directives*)
;;      @eval-always
;;      (defmacro ,name (,label ,@macro-lambda-list)
;;        `(inner (,label)
;; 	  ,,@body))))




