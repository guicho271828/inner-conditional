
(in-package :cl-user)
(defpackage inner-conditional.sample2
  (:use :cl :inner-conditional))
(in-package :inner-conditional.sample2)

(defvar *output-stream* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *sample-label* (gensym)))

(define-inner-condition-with-label
	sample *sample-label* (&body body)
  `(if *output-stream*
	 (,*sample-label* ,@body)
	 (with-output-to-string (*output-stream*)
	   (,*sample-label* ,@body))))

(define-condition-expander *output-stream-definite-here* *sample-label*)

(defun test1 ()
  (let ((*output-stream* t))
	(*output-stream-definite-here*
	  (loop for i from 0 to 5
		   do
		   (sample
			 (format *output-stream* "hello!"))))))

(defun test1 ()
  (*output-stream-definite-here*
	(loop for i from 0 to 5
	   do
		 (sample
		   (format *output-stream* "hello!")))))
