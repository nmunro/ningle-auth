(defpackage ningle-auth/conditions
  (:use :cl)
  (:export #:form-validation-error
           #:form-validation-errors))

(in-package ningle-auth/conditions)

(define-condition form-validation-error (error)
  ((errors :initarg :errors :reader form-validation-errors))
  (:report (lambda (c s)
             (format s "~A" (form-validation-errors c)))))
