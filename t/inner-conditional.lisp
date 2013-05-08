#|
This file is a part of inner-conditional project.
Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inner-conditional-test
  (:use :cl
        :inner-conditional
	:iterate
        :cl-test-more))
(in-package :inner-conditional-test)

(plan nil)

;; blah blah blah.

;; checking the double inners
(defun test0-0 (flag)
  (with-inner (body)
    (iter (for i from 0 to 5)
	  (collecting
	   (with-inner (body2)
	     (iter (for j from 0 to 5)
		   (inner (body2)
		     (if (evenp i)
			 (body2 (collecting (* i j)))
			 (body2 (collecting (+ i j)))))
		   (inner-if body flag
			     (collecting (expt i j))
			     (collecting (expt j i)))))))))

(defun test0-1 (flag)
  (iter (for i from 0 to 5)
	(collecting
	 (iter (for j from 0 to 5)
	       (if (evenp i)
		   (collecting (* i j))
		   (collecting (+ i j)))
	       (if flag
		   (collecting (expt i j))
		   (collecting (expt j i)))))))

(ok (equalp (test0-0 t) (test0-1 t)))
(ok (equalp (test0-0 nil) (test0-1 nil)))


(defun test1 (flag1 flag2)
  (let ((count1 0)
        (count2 0))
    (with-inner (body)
      (iter (for i from 0 to 5)
            (print i)
            (inner (body)
              (if (progn
		    (incf count1)
		    flag1)
		  (body (diag "loop"))
		  (body (diag "loop-not"))))
            (inner-if body (progn
			     (incf count2)
			     flag2)
		      (diag "loop2 on")
		      (diag "loop2 off"))))
    (is count1 1 "the condition is checked only once")
    (is count2 1 "the condition is checked only once")))

(test1 t t)
(test1 t nil)
(test1 nil t)
(test1 nil nil)

(defun test2 (arg)
  (let ((count 0))
    (with-inner (body)
      (iter
        (for i from 0 to 5)
        (inner (body)
          (case (progn (incf count)
                       (mod arg 3))
            (0 (body (format t "divided. i*3 =~a~%"
                             (* i 3))
		     (format t "divided. i*3 =~a~%"
                             (* i 3))))
            (1 (body (format t "modulo 1. i*3 + 1 =~a~%"
                             (+ 1 (* i 3)))))
            (2 (body (format t "modulo 2. i*3 + 2 =~a~%"
                             (+ 2 (* i 3)))))))))
    (is count 1 "the condition is checked only once")))

(test2 0)
(test2 1)
(test2 2)
(test2 3)
(test2 4)
(test2 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(finalize)
