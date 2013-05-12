
(in-package :inner-conditional)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; expansion hook / continuation
(defvar *previous-hooks* nil)
(defun reset-hooks ()
  (setq *previous-hooks* nil
	*macroexpand-hook* 'funcall))
(defun push-hook (hook)
  (let ((prev *macroexpand-hook*))
    (push prev *previous-hooks*)
    (setf *macroexpand-hook*
	  (funcall hook (coerce prev 'function)))))
(defun pop-hook ()
  (setf *macroexpand-hook* (pop *previous-hooks*)))

(defmacro with-hook ((hook) &body body)
  `(unwind-protect
	(progn
	  (push-hook ,hook)
	  ,@body)
     (pop-hook)))
;; (defmacro with-hook ((hook) &body body)
;;   `(prog2
;;        (push-hook ,hook)
;;        (progn ,@body)
;;      (pop-hook)))