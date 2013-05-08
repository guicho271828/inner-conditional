
(in-package :cl-user)
(defpackage inner-conditional.sample
  (:use :cl :inner-conditional :iterate))
(in-package :inner-conditional.sample)

(defvar *output-stream* nil)
(defmacro sample (&body body)
  `(if *output-stream*
       (progn ,@body)
       (with-output-to-string (*output-stream*)
         ,@body)))

(defconstant +loop+ 5000000)

(defun test-speed-without-inner ()
  (with-open-file (*output-stream* "/dev/null"
                                   :direction :output
                                   :if-exists :overwrite)
    (loop for i from 0 to +loop+
       do
         (sample
	  (write-string "hello!" *output-stream*)))
    (loop for i from 0 to +loop+
       do
         (sample
	  (write-string "yep!" *output-stream*)))
    (loop for i from 0 to +loop+
       do
         (sample
	  (write-string "bye!" *output-stream*)))))


(defun test-speed-without-inner-to-string ()
  (loop for i from 0 to +loop+
     do
       (sample
	(write-string "hello!" *output-stream*)))
  (loop for i from 0 to +loop+
     do
       (sample
	(write-string "yep!" *output-stream*)))
  (loop for i from 0 to +loop+
     do
       (sample
	(write-string "bye!" *output-stream*))))

(princ "test without-inner")
(time (test-speed-without-inner))
