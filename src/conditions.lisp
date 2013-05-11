
(in-package :inner-conditional)

(cl-syntax:use-syntax :annot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inner clause
(define-condition compile-time-condition (simple-condition)
  ((form :initarg :form :accessor form)
   (environment :initarg :environment :accessor environment)))

(defmethod print-object ((c compile-time-condition) s)
  (print-unreadable-object (c s)
    (with-slots (form) c
      (format s "~a" form))))