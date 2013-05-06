;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for define-condition-expander
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :inner-conditional)
(cl-syntax:use-syntax :annot)

(defun precompile-1 (sym fn form)
  (walk-tree
   (let ((first t))
     (lambda (subform cont)
       (if first
	   (if (and (consp subform)
		    (equalp sym (car subform)))
	       (progn
		 (setf first nil)
		 (apply fn (cdr subform)))
	       (if (consp subform)
		   (funcall cont subform)
		   subform))
	   subform)))
   form))

(defun expand-single-condition-inner (label version body name
				      version-accumelator)
  (let (condition-body versions template processed-p)
    (with-gensyms (tag)
      (setf template
	    (precompile-1
	     name
	     (lambda (&rest body)
	       (destructuring-bind (condition-body-temp versions-temp)
		   (apply version-accumelator body)
		 (setf processed-p t
		       versions versions-temp
		       condition-body condition-body-temp)
		 (list tag)))
	     (precompile-directives body)))

      (if processed-p
	  (flet ((expand-more-version (version-name)
		   (expand-single-condition-inner
		    label version-name 
		    (precompile-1
		     tag (lambda ()
			   (getf versions version-name))
		     template)
		    name version-accumelator)))
	    (if version
		(expand-more-version version)
		(precompile-1-layer
		 label (lambda (version-name)
			 `(progn ,@(expand-more-version version-name)))
		 condition-body)))
	  template))))


@eval-always
@export
(defmacro define-condition-expander
    ((name expander-name
	   &key (version-expander (gensym)))
     lambda-list &body body)
  (with-gensyms (versions expander-id)
    `(progn
       (defmacro ,expander-name (&body body)
	 (expand-single-condition-inner
	  ',expander-id nil body ',name
	  (lambda ,(subst '&rest '&body lambda-list)
	    (let ((,versions nil))
	      (flet ((,version-expander (id body)
		       (setf (getf ,versions id) body)
		       `(,',expander-id ,id)))
		(let ((condition-body ,@body))
		  (list
		   condition-body
		   ,versions)))))))

       (defmacro ,name ,lambda-list
	 (let ((,versions nil))
	   (flet ((,version-expander (id body)
		    (setf (getf ,versions id) body)
		    `(,',expander-id ,id)))
	     (let ((condition-body ,@body))
	       (precompile-1-layer
		',expander-id
		(lambda (version-name)
		  (getf ,versions version-name))
		condition-body))))))))
