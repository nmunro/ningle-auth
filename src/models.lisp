(defpackage ningle-auth/models
  (:use :cl :mito)
  (:import-from :mito-auth
                :password-hash)
  (:export #:user
           #:id
           #:created-at
           #:updated-at
           #:email
           #:username
           #:password-hash
           #:role
           #:permission
           #:token
           #:token-value
           #:generate-token
           #:is-expired-p
           #:activate
           #:+email-verification+
           #:+password-reset+
           #:+token-purposes+))

(in-package ningle-auth/models)

(defparameter +email-verification+ "email-verification")
(defparameter +password-reset+ "password-reset")
(defparameter +token-purposes+ (list +email-verification+ +password-reset+))

(deftable user (mito-auth:has-secure-password)
  ((email    :col-type (:varchar 255) :initarg  :email    :accessor email)
   (username :col-type (:varchar 255) :initarg  :username :accessor username)
   (active   :col-type :integer       :initform 0         :accessor active))
  (:unique-keys email username))

(deftable role ()
  ((name        :col-type (:varchar 255)  :initarg :name        :accessor name)
   (description :col-type (:varchar 2048) :initarg :description :accessor description))
  (:unique-keys name))

(deftable permission ()
  ((user :col-type user :references (user id))
   (role :col-type role :references (role id)))
  (:unique-keys (user role)))

(deftable token ()
  ((user       :col-type user          :references (user id))
   (purpose    :col-type :string       :initarg :purpose    :accessor token-purpose)
   (token      :col-type (:varchar 64) :initarg :token      :accessor token-value)
   (salt       :col-type :binary       :accessor token-salt)
   (expires-at :col-type :timestamp    :accessor token-expires-at))
  (:unique-keys (user-id purpose)))

(defgeneric activate (user)
  (:documentation "Set the active slot of a user to 1"))

(defmethod activate ((user user))
  (setf (active user) 1))

(defgeneric is-expired-p (token)
  (:documentation "Determines if a token has expired"))

(defmethod is-expired-p ((token token))
  (let ((expiry (token-expires-at token)))
    (typecase expiry
      (local-time:timestamp
       (> (get-universal-time) (local-time:timestamp-to-universal expiry)))

      (integer
       (> (get-universal-time) expiry))

      (t
       (error "Unknown type for token-expires-at: ~S" (type-of expiry))))))

(defmethod initialize-instance :before ((token token) &rest initargs &key purpose &allow-other-keys)
  (unless (member purpose +token-purposes+ :test #'string=)
    (error "Invalid token purpose: ~A. Allowed: ~A" purpose +token-purposes+)))

(defmethod initialize-instance :after ((token token) &rest initargs &key &allow-other-keys)
  (unless (slot-boundp token 'salt)
    (setf (token-salt token) (ironclad:make-random-salt 16)))

  (unless (slot-boundp token 'expires-at)
    (setf (token-expires-at token) (+ (get-universal-time) (envy-ningle:get-config :token-expiration)))))

(defgeneric generate-token (user purpose &key expires-in)
  (:documentation "Generates a token for a user"))

(defmethod generate-token ((user user) purpose &key (expires-in (envy-ningle:get-config :token-expiration)))
    (unless (member purpose +token-purposes+ :test #'string=)
      (error "Invalid token purpose: ~A. Allowed: ~A" purpose +token-purposes+))

    (let* ((salt (ironclad:make-random-salt 16))
           (expires-at (truncate (+ (get-universal-time) expires-in)))
           (base-string (format nil "~A~A~A" (username user) expires-at salt))
           (hash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (babel:string-to-octets base-string)))))
        (create-dao 'token :user user :purpose purpose :token hash :salt salt :expires-at expires-at)))
