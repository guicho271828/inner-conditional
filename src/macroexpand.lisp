

(in-package :restartable-macroexpand)

(cl-syntax:use-syntax :annot)


(defun my-macroexpand-1 (form &optional env)
  (restart-case
      (funcall *cl-macroexpand-1* form env)
    (use-value (form)
      :test (lambda (c) (typep c 'compile-time-condition))
      form)))

(defun enable-macroexpand-restart ()
  )
(defun disable-macroexpand-restart ()
  )