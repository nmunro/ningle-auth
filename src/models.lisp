(defpackage ningle-auth/models
  (:use :cl :mito)
  (:import-from :mito-auth
                :password-hash)
  (:export #:user
           #:email
           #:username
           #:password-hash
           #:role
           #:permission
           #:create-super-user))

(in-package ningle-auth/models)

(deftable user (mito-auth:has-secure-password)
  ((email    :col-type (:varchar 255) :initarg :email    :accessor email)
   (username :col-type (:varchar 255) :initarg :username :accessor username))
  (:unique-keys email username))

(deftable role ()
  ((name        :col-type (:varchar 255)  :initarg :name        :accessor name)
   (description :col-type (:varchar 2048) :initarg :description :accessor description))
  (:unique-keys name))

(deftable permission ()
  ((user :col-type user :references (user id))
   (role :col-type role :references (role id)))
  (:unique-keys (user role)))
