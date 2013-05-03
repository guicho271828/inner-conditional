
(in-package :cl-user)
(defpackage inner-conditional.sample2
  (:use :cl :inner-conditional :iterate))
(in-package :inner-conditional.sample2)

(defvar *output-stream* nil)

(define-condition-expander
    (sample *output-stream-definite-here*
            :version-expander version)  
    (&body body)
  `(if *output-stream*
       ,(version 'stream `(progn ,@body))
       (with-output-to-string (*output-stream*)
		 ,(version 'no-stream `(progn ,@body)))))

(defconstant +loop+ 50000000)

(defun test-speed-with-inner ()
  (with-open-file (*output-stream* "/dev/null"
								   :direction :output
								   :if-exists :overwrite)
	(*output-stream-definite-here*
	  (iter (for i from 0 to +loop+)
			(sample
			  (write-string "hello!" *output-stream*)))
	  (iter (for i from 0 to +loop+)
			(sample
			  (write-string "yep!" *output-stream*)))
	  (iter (for i from 0 to +loop+)
			(sample
			  (write-string "bye!" *output-stream*))))))

(princ "test - with-inner")
(time (test-speed-with-inner))

(defun test-speed-out-of-expander ()
  (with-open-file (*output-stream* "/dev/null"
								   :direction :output
								   :if-exists :overwrite)
	(iter (for i from 0 to +loop+)
		  (sample
			(write-string "hello!" *output-stream*)))
	(iter (for i from 0 to +loop+)
		  (sample
			(write-string "yep!" *output-stream*)))
	(iter (for i from 0 to +loop+)
		  (sample
			(write-string "bye!" *output-stream*)))))

(princ "test - conditionals out of expander")
(time (test-speed-out-of-expander))

(defun test0 ()
  (let ((*output-stream* t))
    (*output-stream-definite-here*
      (iter (for i from 0 to 5)
			(sample
			  (format *output-stream* "hello!"))))))

(defun test1 ()
  (*output-stream-definite-here*
    (iter (for i from 0 to 5)
		  (sample
			(format *output-stream* "hello!")))))

(defun test2 ()
  (*output-stream-definite-here*
    (iter (for i from 0 to 5)
		  (sample
			(format *output-stream* "hello!")))
    (iter (for i from 0 to 5)
		  (sample
			(format *output-stream* "bye!")))))

(test0)
(test1)
(test2)