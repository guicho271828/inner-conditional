
(in-package :cl-user)
(defpackage inner-conditional.sample2
  (:use :cl :inner-conditional :iterate :cl-test-more))
(in-package :inner-conditional.sample2)

(defvar *output-stream* nil)

(define-condition-expander
    (sample *output-stream-definite-here* version)
    (&body body)
  `(if *output-stream*
       (version ,@body)
       (with-output-to-string (*output-stream*)
	 (version ,@body))))

(defparameter +loop+ 300)

(defun test0-0 ()
  (declare (optimize (speed 3)))
  (diag "test with *output-stream-definite-here*")
  (with-output-to-string (*output-stream*)
    (*output-stream-definite-here*
      (iter
	(for i below +loop+)
	(iter
	  (for j below +loop+)
	  (iter
	    (for k below +loop+)
	    (sample
	      (write-char #\. *output-stream*))))))))

(defun test0-1 ()
  (declare (optimize (speed 3)))
  (diag "test without *output-stream-definite-here*")
  (with-output-to-string (*output-stream*)
    (iter
      (for i below +loop+)
      (iter
	(for j below +loop+)
	(iter
	  (for k below +loop+)
	  (sample
	    (write-char #\. *output-stream*)))))))

(ok (string= (time (test0-0))
	     (time (test0-1))))