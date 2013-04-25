
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
