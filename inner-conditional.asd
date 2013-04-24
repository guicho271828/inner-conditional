#|
  This file is a part of inner-conditional project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Series of macros which optimizes out the inner conditional jumping

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inner-conditional-asd
  (:use :cl :asdf))
(in-package :inner-conditional-asd)

(defsystem inner-conditional
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:iterate
               :alexandria
               :cl-syntax
			   :cl-annot
			   :optima)
  :components ((:module "src"
                :components
                ((:file "inner-conditional"))))
  :description "Series of macros which optimizes out the inner conditional jumping"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op inner-conditional-test))))
