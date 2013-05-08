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

(with-inner (body)
  (dotimes (i 5 5)
    (inner (body)
      (if flag
	  (body (princ "loop on"))
	  (body (princ "loop off"))))))

(defun test0 (flag)
  (with-inner (body)
    (iter (for i from 0 to 5)
	  (with-inner (body2)
            (iter (for j from 0 to 5)
		  (format t "~%i: ~a j: ~a" i j)
		  (inner (body2)
		    (if (evenp i)
			(body2 (format t "  i is even"))
			(body2 (format t "  i is odd"))))
		  (inner-if body flag
			    (format t "  loop on")
			    (format t "  loop off")))))))

(test0 t)
(test0 nil)

(defun test1 (flag1 flag2)
  (let ((count1 0)
        (count2 0))
    (with-inner (body)
      (iter (for i from 0 to 5)
            (print i)
            (inner (body)
              (when (progn
                      (incf count1)
                      flag1)
                (body (diag "loop"))))
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
