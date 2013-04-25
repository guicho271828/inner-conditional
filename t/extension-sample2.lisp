
(in-package :cl-user)
(defpackage inner-conditional.sample2
  (:use :cl :inner-conditional))
(in-package :inner-conditional.sample2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sample-label* 'sample-label))

(defvar *output-stream* nil)

(define-condition-expander
	sample *sample-label* *output-stream-definite-here*
	(:force-single-check t)
	(&body body)
  `(if *output-stream*
	 (,*sample-label* ,@body)
	 (with-output-to-string (*output-stream*)
	   (,*sample-label* ,@body))))

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

(defun test2 ()
  (*output-stream-definite-here*
	(loop for i from 0 to 5
	   do
		 (sample
		   (format *output-stream* "hello!")))
	(loop for i from 0 to 5
	   do
		 (sample
		   (format *output-stream* "hello!")))))
