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
           #:assign-role
           #:revoke-role
           #:has-role-p
           #:is-admin-p
           #:on-activate
           #:on-register
           #:on-reset
           #:on-delete
           #:destroy
           ;; Re-export from token-registry for convenience
           #:+email-verification+
           #:+password-reset+
           #:*default-roles*))

(in-package ningle-auth/models)

(defparameter *default-roles* nil)

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

(defun build-url-root (&key (path ""))
  (let ((scheme (lack/request:request-uri-scheme ningle:*request*))
        (name (lack/request:request-server-name ningle:*request*))
        (port (lack/request:request-server-port ningle:*request*)))
    (if (or (= port 80) (= port 443))
      (format nil "~(~A~)://~(~A~)~A" scheme name path)
      (format nil "~(~A~)://~(~A~):~A~A" scheme name port path))))

(defun build-activation-link (user token)
  (let ((host (build-url-root :path (envy-ningle:get-config :auth-mount-path))))
    (format nil "~A/verify?user=~A&token=~A" host (username user) (value token))))

(defun build-reset-link (user token)
  (let ((host (build-url-root :path (envy-ningle:get-config :auth-mount-path))))
    (format nil "~A/reset/process?user=~A&token=~A" host (username user) (value token))))

(defgeneric activate (user)
  (:documentation "Set the active slot of a user to 1")
  (:method ((user user))
    (setf (active user) 1)))

(defgeneric is-expired-p (token)
  (:documentation "Determines if a token has expired")
  (:method ((token token))
    (let ((expiry (expires-at token)))
      (typecase expiry
        (local-time:timestamp
         (> (get-universal-time) (local-time:timestamp-to-universal expiry)))

        (integer
         (> (get-universal-time) expiry))

        (t
         (error "Unknown type for token-expires-at: ~S" (type-of expiry)))))))

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
  (:documentation "Generates a token for a user")
  (:method ((user user) purpose &key (expires-in (envy-ningle:get-config :token-expiration)))
    (unless (token-purpose-valid-p purpose)
      (error "Invalid token purpose: ~A. Use register-token-purpose to register new purposes." purpose))

    (let* ((salt (ironclad:make-random-salt 16))
           (expires-at (truncate (+ (get-universal-time) expires-in)))
           (base-string (format nil "~A~A~A" (username user) expires-at salt))
           (hash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (babel:string-to-octets base-string)))))
        (create-dao 'token :user user :purpose purpose :token hash :salt salt :expires-at expires-at))))

(defgeneric assign-role (user role)
  (:documentation "Attaches a role to a user")
  (:method ((user user) (role role))
    (alexandria:if-let ((permission (mito:find-dao 'permission :user user :role role)))
      permission
      (mito:create-dao 'permission :user user :role role)))

  (:method ((user user) (role string))
    (alexandria:when-let ((role (mito:find-dao 'role :name role)))
      (assign-role user role))))

(defgeneric revoke-role (user role)
  (:documentation "Revokes a role from a user")
  (:method ((user user) (role role))
    (alexandria:when-let ((perm (mito:find-dao 'permission :user user :role role)))
      (mito:delete-dao perm)))

  (:method ((user user) (role string))
    (alexandria:when-let ((role (mito:find-dao 'role :name role)))
      (revoke-role user role))))

(defgeneric has-role-p (user role)
  (:documentation "Checks if a user has a given role")
  (:method ((user user) (role role))
    (mito:find-dao 'permission :user user :role role))

  (:method ((user user) (role string))
    (alexandria:when-let ((role (mito:find-dao 'role :name role)))
      (has-role-p user role))))

(defgeneric is-admin-p (user)
  (:documentation "Checks if a user is an admin")
  (:method ((user user))
    (has-role-p user "admin")))

(defgeneric on-register (user)
  (:documentation "Called when a user is registered")
  (:method ((user user))
    (let* ((token (generate-token user +email-verification+))
           (link (build-activation-link user token))
           (subject (format nil "~A registration for ~A" (envy-ningle:get-config :project-name) (username user)))
           (template "ningle-auth/email/register.txt")
           (content (djula:render-template* template nil :user user :link link)))
      (ningle-email:send-mail subject content (email user)))))

(defgeneric on-activate (user)
  (:documentation "Called when a user is activated")
  (:method ((user user))
    (dolist (role *default-roles*)
      (assign-role user role))

    (activate user)
    (mito:save-dao user)))

(defgeneric on-reset (user)
  (:documentation "Called when a user is reset")
  (:method ((user user))
    (let* ((token (generate-token user +password-reset+))
           (link (build-reset-link user token))
           (subject (format nil "~A password reset for ~A" (envy-ningle:get-config :project-name) (username user)))
           (template "ningle-auth/email/reset.txt")
           (content (djula:render-template* template nil :user user :link link)))
        (ningle-email:send-mail subject content (email user)))))

(defgeneric on-delete (user)
  (:documentation "Called when a user is deleted")
  (:method ((user user))
    (dolist (perm (mito:select-dao 'permission :user-id (mito:object-id user)))
      (mito:delete-dao perm))
    (mito:delete-dao user))

  (:method ((role role))
    nil))

(defun destroy (object)
  (on-delete object))
