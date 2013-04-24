#|
  This file is a part of inner-conditional project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inner-conditional-test-asd
  (:use :cl :asdf))
(in-package :inner-conditional-test-asd)

(defsystem inner-conditional-test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:inner-conditional
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "inner-conditional"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
