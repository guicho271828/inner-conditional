
#|
  This file is a part of inner-conditional project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inner-conditional
  (:use :cl :optima :iterate
	:annot.doc :annot.eval-when
	:alexandria
	:macroexpand-dammit))
