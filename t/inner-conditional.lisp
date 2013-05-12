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

;; two layers
(defun test0-0 (flag)
  (with-inner (body)
    (iter (for i from 0 to 5)
	  (collecting
	   (with-inner (body2)
	     (iter (for j from 0 to 5)
		   (collecting
		    (inner (body2)
		      (if (evenp i)
			  (body2 (* i j))
			  (body2 (+ i j)))))
		   (collecting
		    (inner-if body flag
			      (expt i j)
			      (expt j i)))))))))

(defun test0-1 (flag)
  (iter (for i from 0 to 5)
	(collecting
	 (iter (for j from 0 to 5)
	       (collecting
		(if (evenp i)
		    (* i j)
		    (+ i j)))
	       (collecting
		(if flag
		    (expt i j)
		    (expt j i)))))))

(ok (equalp (test0-0 t) (test0-1 t)))
(ok (equalp (test0-0 nil) (test0-1 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; two inners in the same layer
(defun test1-0 (flag1 flag2)
  (let ((count1 0)
        (count2 0))
    (prog1
	(with-inner (body)
	  (iter (for i from 0 to 5)
		(collecting
		 (inner (body)
		   (if (progn
			 (incf count1)
			 flag1)
		       (body "loop")
		       (body "loop-not"))))
		(collecting
		 (inner-if body (progn
				  (incf count2)
				  flag2)
			   "loop2 on"
			   "loop2 off"))))
      (is count1 1 "the condition is checked only once")
      (is count2 1 "the condition is checked only once"))))

(defun test1-1 (flag1 flag2)
  (iter (for i from 0 to 5)
	(collecting
	 (if flag1
	     "loop"
	     "loop-not"))
	(collecting
	 (if flag2
	     "loop2 on"
	     "loop2 off"))))

(iter (for f1 in '(t nil))
      (iter (for f2 in '(t nil))
	    (ok (equalp (test1-0 f1 f2) (test1-1 f1 f2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case
(defun test2-0 (arg)
  (let ((count 0))
    (prog1
	(with-inner (body)
	  (iter
	    (for i from 0 to 5)
	    (collecting
	     (inner (body)
	       (case (progn (incf count)
			    (mod arg 3))
		 (0 (body (* i 3)))
		 (1 (body (+ 1 (* i 3))))
		 (2 (body (+ 2 (* i 3)))))))))
      (is count 1 "the condition is checked only once"))))

(defun test2-1 (arg)
  (let ((count 0))
    (iter
      (for i from 0 to 5)
      (collecting
       (case (progn (incf count)
		    (mod arg 3))
	 (0 (* i 3))
	 (1 (+ 1 (* i 3)))
	 (2 (+ 2 (* i 3))))))))

(iter (for i below 27)
      (ok (equalp (test2-0 i) (test2-1 i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing the macrolet compatibility

(defun test3-0 (flag)
  (print
   (symbol-macrolet ((a i))
     (with-inner (body)
       (loop for i from 0 to 5
	  collecting
	    (with-inner (body2)
	      (symbol-macrolet ((b j))
		(loop for j from 0 to 5
		   collecting
		     (inner (body2)
		       (macrolet ((e (op) `(,op a b)))
			 (if (evenp i)
			     (body2 (e *))
			     (body2 (e +)))))
		   collecting
		     (inner-if body flag
			       (expt a b)
			       (expt b a))))))))))

(defun test3-1 (flag)
  (print
   (symbol-macrolet ((a i))
     (with-inner (body)
       (loop for i from 0 to 5
	  collecting
	    (macrolet ((iif (&rest args)
			 (print :iif-expanded)
			 `(inner-if ,@args)))
	      (with-inner (body2)
		(symbol-macrolet ((b j))
		  (loop for j from 0 to 5
		     collecting
		       (macrolet ((e (op) `(,op a b)))
			 (iif body2 (evenp i)
			      (e *)
			      (e +)))
		     collecting
		       (iif body flag
			    (expt a b)
			    (expt b a)))))))))))

(defun test3-2 (flag)
  (print
   (symbol-macrolet ((a i))
     (loop for i from 0 to 5
	collecting
	  (symbol-macrolet ((b j))
	    (loop for j from 0 to 5
	       collecting
		 (macrolet ((e (op) `(,op a b)))
		   (if (evenp i)
		       (e *)
		       (e +)))
	       collecting
		 (if flag
		     (expt a b)
		     (expt b a))))))))

(ok (equalp (test3-0 t) (test3-1 t))
    "macrolet-test0")
(ok (equalp (test3-0 nil) (test3-1 nil))
    "macrolet-test1")
(ok (equalp (test3-0 t) (test3-2 t))
    "macrolet-test0")
(ok (equalp (test3-0 nil) (test3-2 nil))
    "macrolet-test1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test4-1 (i j)
  (symbol-macrolet ((a i))
    (with-inner (body)
      (macrolet ((iif (&rest args)
		   (print :iif-expanded)
		   `(inner-if ,@args)))
	(with-inner (body2)
	  (symbol-macrolet ((b j))
	    (macrolet ((e (op) `(,op a b)))
	      (iif body2 (evenp i)
		   (e *)
		   (e +)))))))))

(finalize)
