(defpackage ningle-auth/models
  (:use :cl :mito)
  (:import-from :mito-auth
                :password-hash)
  (:import-from :ningle-auth/token-registry
                #:token-purpose-valid-p
                #:+email-verification+
                #:+password-reset+)
  (:export #:user
           #:id
           #:created-at
           #:updated-at
           #:expires-at
           #:email
           #:username
           #:active
           #:activate
           #:name
           #:description
           #:role
           #:password-hash
           #:permission
           #:purpose
           #:salt
           #:token
           #:value
           #:generate-token
           #:is-expired-p
           ;; Re-export from token-registry for convenience
           #:+email-verification+
           #:+password-reset+))

(in-package ningle-auth/models)

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
   (purpose    :col-type :text         :initarg :purpose :accessor purpose)
   (token      :col-type (:varchar 64) :initarg :token   :accessor value)
   (salt       :col-type :binary       :accessor salt)
   (expires-at :col-type :timestamp    :accessor expires-at))
  (:unique-keys (user-id purpose)))

(defgeneric activate (user)
  (:documentation "Set the active slot of a user to 1"))

(defmethod activate ((user user))
  (setf (active user) 1))

(defgeneric is-expired-p (token)
  (:documentation "Determines if a token has expired"))

(defmethod is-expired-p ((token token))
  (let ((expiry (expires-at token)))
    (typecase expiry
      (local-time:timestamp
       (> (get-universal-time) (local-time:timestamp-to-universal expiry)))

      (integer
       (> (get-universal-time) expiry))

      (t
       (error "Unknown type for token-expires-at: ~S" (type-of expiry))))))

(defmethod initialize-instance :before ((token token) &rest initargs &key purpose &allow-other-keys)
  (declare (ignore initargs))
  (unless (token-purpose-valid-p purpose)
    (error "Invalid token purpose: ~A. Use register-token-purpose to register new purposes." purpose)))

(defmethod initialize-instance :after ((token token) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (unless (slot-boundp token 'salt)
    (setf (salt token) (ironclad:make-random-salt 16)))

  (unless (slot-boundp token 'expires-at)
    (setf (expires-at token) (+ (get-universal-time) (envy-ningle:get-config :token-expiration)))))

(defgeneric generate-token (user purpose &key expires-in)
  (:documentation "Generates a token for a user"))

(defmethod generate-token ((user user) purpose &key (expires-in (envy-ningle:get-config :token-expiration)))
    (unless (token-purpose-valid-p purpose)
      (error "Invalid token purpose: ~A. Use register-token-purpose to register new purposes." purpose))

    (let* ((salt (ironclad:make-random-salt 16))
           (expires-at (truncate (+ (get-universal-time) expires-in)))
           (base-string (format nil "~A~A~A" (username user) expires-at salt))
           (hash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (babel:string-to-octets base-string)))))
        (create-dao 'token :user user :purpose purpose :token hash :salt salt :expires-at expires-at)))
