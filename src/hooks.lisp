
(in-package :restartable-macroexpand)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; expansion hook / continuation
(defvar *previous-hooks* nil)
(defun reset-hooks ()
  (setq *previous-hooks* nil
	*macroexpand-hook* 'funcall))
(defun push-hook (hook)
  (push *macroexpand-hook* *previous-hooks*)
  (setf *macroexpand-hook* hook))
(defun pop-hook ()
  (setf *macroexpand-hook* (pop *previous-hooks*)))

