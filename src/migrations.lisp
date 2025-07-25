(defpackage ningle-auth/migrations
  (:use :cl :mito)
  (:export #:migrate))

(in-package :ningle-auth/migrations)

(defun migrate ()
  "Explicitly apply migrations when called."
  (format t "Applying migrations...~%")
  (mito:ensure-table-exists 'ningle-auth/models:user)
  (mito:ensure-table-exists 'ningle-auth/models:role)
  (mito:ensure-table-exists 'ningle-auth/models:permission)
  (mito:ensure-table-exists 'ningle-auth/models:token)
  (mito:migrate-table 'ningle-auth/models:user)
  (mito:migrate-table 'ningle-auth/models:role)
  (mito:migrate-table 'ningle-auth/models:permission)
  (mito:migrate-table 'ningle-auth/models:token)
  (create-dao 'ningle-auth/models:role :name "admin" :description "Admin")
  (create-dao 'ningle-auth/models:role :name "user" :description "User")
  (format t "Migrations complete.~%"))
