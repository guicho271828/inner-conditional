
(in-package :cl-user)
(defpackage inner-conditional.sample
  (:use :cl :inner-conditional))
(in-package :inner-conditional.sample)

(defvar *output-stream* nil)
(defmacro sample (&body body)
  `(if *output-stream*
       (progn ,@body)
       (with-output-to-string (*output-stream*)
         ,@body)))

(defun test-speed-without-inner ()
  (loop for i from 0 to 5000000
	 do
	   (sample
		 (write-string "hello!" *output-stream*)))
  (loop for i from 0 to 5000000
	 do
	   (sample
		 (write-string "yep!" *output-stream*)))
  (loop for i from 0 to 5000000
	 do
	   (sample
		 (write-string "bye!" *output-stream*))))

(princ "test without-inner")
(time (test-speed-without-inner))
