(defpackage ningle-auth/migrations
  (:use :cl :mito)
  (:export #:migrate))

(in-package :ningle-auth/migrations)

(defun migrate ()
  "Explicitly apply migrations when called."
  (format t "Applying migrations...~%")
  (multiple-value-bind (backend args) (envy-ningle:extract-mito-config :ningle-auth/config)
    (unless backend
      (error "No :mito middleware config found in ENVY config."))
    (apply #'mito:connect-toplevel backend args)
    (mito:ensure-table-exists 'ningle-auth/models:user)
    (mito:migrate-table 'ningle-auth/models:user)
    (mito:disconnect-toplevel)
    (format t "Migrations complete.~%")))
