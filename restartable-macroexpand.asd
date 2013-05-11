#|
This file is a part of inner-conditional project.
Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
Series of macros which optimizes out the inner conditional jumping

Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage restartable-macroexpand-asd
  (:use :cl :asdf))
(in-package :restartable-macroexpand-asd)

(defsystem restartable-macroexpand
  :version "0.1.0"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:alexandria
	       :cl-syntax-annot)
  :components ((:module "src"
			:serial t
			:components
			((:file :restartable-macroexpand)
			 (:file :conditions)
			 (:file :hooks)
			 (:file :macroexpand))))
  :description "replaces cl:macroexpand-1 and provide a restart facility"
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
  :in-order-to ((test-op (load-op restartable-macroexpand-test))))
