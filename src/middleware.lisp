(defpackage ningle-auth/middleware
  (:use :cl :envy-ningle)
  (:export #:refresh-roles))

(in-package ningle-auth/middleware)

(defun refresh-roles (app)
  (lambda (env)
    (multiple-value-bind (backend args)
        (extract-mito-config :ningle-tutorial-project/config)
      (unwind-protect
           (progn
             (apply #'mito:connect-toplevel backend args)
             (format t "[refresh-roles] result: ~A~%" (mito:retrieve-dao 'ningle-auth/models:role)))
        (mito:disconnect-toplevel)))
    (funcall app env)))
