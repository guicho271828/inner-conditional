
(in-package :cl-user)
(defpackage inner-conditional.sample2
  (:use :cl :inner-conditional :iterate))
(in-package :inner-conditional.sample2)

(defvar *output-stream* nil)

(define-condition-expander
    (sample *output-stream-definite-here* version)
    (&body body)
  `(if *output-stream*
       (version ,@body)
       (with-output-to-string (*output-stream*)
	 (version ,@body))))

(defconstant +loop+ 100000000)

(defun test-speed-with-inner ()
  (declare (optimize (speed 3)))
  (with-open-file (*output-stream* "/dev/null"
				   :direction :output
				   :if-exists :overwrite)
    (*output-stream-definite-here*
      (loop for i from 0 to +loop+
	 do (sample
	      (write-string "hello!" *output-stream*)))
      (loop for i from 0 to +loop+
	 do
	   (sample
	     (write-string "yep!" *output-stream*)))
      (loop for i from 0 to +loop+
	 do
	   (sample
	     (write-string "bye!" *output-stream*))))))

(princ "test - with-inner")
(time (test-speed-with-inner))

(defun test-speed-out-of-expander ()
  (declare (optimize (speed 3)))
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

(princ "test - conditionals out of expander")
(time (test-speed-out-of-expander))
