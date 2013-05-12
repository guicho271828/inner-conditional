
(in-package :cl-user)
(defpackage inner-conditional.sample
  (:use :cl :inner-conditional :iterate :cl-test-more))
(in-package :inner-conditional.sample)

(defvar *output-stream* nil)
(defmacro sample (&body body)
  `(if *output-stream*
       (progn ,@body)
       (with-output-to-string (*output-stream*)
         ,@body)))

(defparameter +loop+ 300)

(defun test0-0 ()
  (declare (optimize (speed 3)))
  (diag "test without aid of define-condition-expander")
  (with-output-to-string (*output-stream*)
    (iter
      (for i below +loop+)
      (iter
	(for j below +loop+)
	(iter
	  (for k below +loop+)
	  (sample
	    (write-char #\. *output-stream*)))))))

(time (test0-0))