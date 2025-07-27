(defpackage ningle-auth
  (:use :cl :sxql :ningle-auth/forms)
  (:export #:*app*
           #:*config*
           #:start
           #:stop
           #:get-config
           #:set-config))

(in-package ningle-auth)

(defvar *app* (make-instance 'ningle:app))
(defvar *config* nil)

(defun set-config (config)
  (setf *config* config))

(defun get-config (&optional (key nil keyp))
  (if keyp
      (getf *config* key)
      *config*))

(cu-sith:setup
    :user-p (lambda (username) (mito:find-dao 'ningle-auth/models:user :username username :active 1))
    :user-roles (lambda (user) (mito:select-dao 'ningle-auth/models:permission (where (:= :user_id (mito:object-id user))))))

(djula:add-template-directory (asdf:system-relative-pathname :ningle-auth "src/templates/"))

(setf (ningle:route *app* "/register" :method '(:GET :POST))
    (lambda (params)
        (let ((form (cl-forms:find-form 'register)))
          (if (string= "GET" (lack.request:request-method ningle:*request*))
            (djula:render-template* "ningle-auth/register.html" nil :title "Register" :form form)
            (handler-case
                (progn
                    (cl-forms:handle-request form) ; Can throw an error if CSRF fails
                    (multiple-value-bind (valid errors)
                        (cl-forms:validate-form form)

                      (when errors
                        (format t "Errors: ~A~%" errors))

                      (when valid
                        (cl-forms:with-form-field-values (email username password password-verify) form
                          (when (mito:select-dao 'ningle-auth/models:user
                                 (where (:or (:= :username username)
                                             (:= :email email))))
                            (error "Either username or email is already registered"))

                          (when (string/= password password-verify)
                            (error "Passwords do not match"))

                          (let* ((user (mito:create-dao 'ningle-auth/models:user :email email :username username :password password))
                                 (token (ningle-auth/models:generate-token user ningle-auth/models:+email-verification+)))
                            (format t "Reset url: ~A~A/verify?user=~A&token=~A~%"
                                            (format nil "http://~A:~A" (lack/request:request-server-name ningle:*request*) (lack/request:request-server-port ningle:*request*))
                                            (get-config :mount-path)
                                            (ningle-auth/models:username user)
                                            (ningle-auth/models:token-value token))
                            (ingle:redirect "/"))))))

                (error (err)
                    (djula:render-template* "error.html" nil :title "Error" :error err))

                (simple-error (csrf-error)
                    (setf (lack.response:response-status ningle:*response*) 403)
                    (djula:render-template* "error.html" nil :title "Error" :error csrf-error)))))))

;; Must be logged out
(setf (ningle:route *app* "/login" :method '(:GET :POST))
    (lambda (params)
        (let ((form (cl-forms:find-form 'login)))
            (if (string= "GET" (lack.request:request-method ningle:*request*))
                (djula:render-template* "ningle-auth/login.html" nil :form form :url (concatenate 'string (get-config :mount-path) "/reset"))
                (handler-case
                    (progn
                        (cl-forms:handle-request form) ; Can throw an error if CSRF fails

                        (multiple-value-bind (valid errors)
                            (cl-forms:validate-form form)

                          (when errors
                            (format t "Errors: ~A~%" errors))

                          (when valid
                            (cl-forms:with-form-field-values (username password) form
                                (cu-sith:login :user username :password password)
                                (ingle:redirect (get-config :login-redirect))))))

                    (cu-sith:invalid-user (err)
                        (djula:render-template* "error.html" nil :title "Error" :error (format nil "~A, have you verified the account?" (cu-sith:msg err))))

                    (cu-sith:invalid-password (err)
                        (djula:render-template* "error.html" nil :title "Error" :error (cu-sith:msg err)))

                    (simple-error (csrf-error)
                        (setf (lack.response:response-status ningle:*response*) 403)
                        (djula:render-template* "error.html" nil :title "Error" :error csrf-error)))))))


;; Must be logged in
(setf (ningle:route *app* "/logout" :method '(:GET :POST))
    (lambda (params)
        (cu-sith:logout)
        (ingle:redirect (get-config :login-redirect))))

;; Must be logged out
(setf (ningle:route *app* "/reset" :method '(:GET :POST))
    (lambda (params)
        (let ((form (cl-forms:find-form 'reset-password)))
            (cond
              ((cu-sith:logged-in-p)
                (setf (lack.response:response-status ningle:*response*) 403)
                (djula:render-template* "error.html" nil :title "Error" :error "Cannot reset password while logged in"))

              ((string= "GET" (lack.request:request-method ningle:*request*))
                (djula:render-template* "ningle-auth/reset.html" nil :title "Reset GET" :form form))

              (t
                (handler-case
                    (progn
                        (cl-forms:handle-request form) ; Can throw an error if CSRF fails
                        (multiple-value-bind (valid errors)
                            (cl-forms:validate-form form)

                          (when errors
                            (format t "Errors: ~A~%" errors))

                          (when valid
                            (cl-forms:with-form-field-values (email) form
                                (let* ((user (mito:find-dao 'ningle-auth/models:user :email email))
                                       (token (mito:find-dao 'ningle-auth/models:token :user user :purpose ningle-auth/models:+password-reset+)))
                                  (cond
                                    ((and user token (not (ningle-auth/models:is-expired-p token)))
                                        (djula:render-template* "error.html" nil :title "Error" :error "There is already a password reset in progress, either continue or wait a while before retrying"))

                                    ((and user token)
                                        (mito:delete-dao token)
                                        (let ((token (ningle-auth/models:generate-token user ningle-auth/models:+password-reset+)))
                                          (format t "Reset url: ~A~A/reset/process?user=~A&token=~A~%"
                                            (format nil "http://~A:~A" (lack/request:request-server-name ningle:*request*) (lack/request:request-server-port ningle:*request*))
                                            (get-config :mount-path)
                                            (ningle-auth/models:username user)
                                            (ningle-auth/models:token-value token)))
                                        (ingle:redirect "/"))

                                    (user
                                        (let ((token (ningle-auth/models:generate-token user ningle-auth/models:+password-reset+)))
                                          (format t "Reset url: ~A~A/reset/process?user=~A&token=~A~%"
                                            (format nil "http://~A:~A" (lack/request:request-server-name ningle:*request*) (lack/request:request-server-port ningle:*request*))
                                            (get-config :mount-path)
                                            (ningle-auth/models:username user)
                                            (ningle-auth/models:token-value token)))
                                        (ingle:redirect "/"))

                                    (t
                                     (djula:render-template* "error.html" nil :title "Error" :error "No user found"))))))))

                    (simple-error (csrf-error)
                        (setf (lack.response:response-status ningle:*response*) 403)
                        (djula:render-template* "error.html" nil :title "Error" :error csrf-error))))))))

(setf (ningle:route *app* "/reset/process" :method '(:GET :POST))
      (lambda (params)
        (let* ((form (cl-forms:find-form 'new-password))
               (user (mito:find-dao 'ningle-auth/models:user :username (cdr (assoc "user" params :test #'string=))))
               (token (mito:find-dao 'ningle-auth/models:token :user user :purpose ningle-auth/models:+password-reset+ :token (cdr (assoc "token" params :test #'string=)))))
          (cond
            ((and (string= "GET" (lack.request:request-method ningle:*request*)) (or (not token) (ningle-auth/models:is-expired-p token)))
                (djula:render-template* "error.html" nil :title "Error" :error "Invalid reset token, please try again"))

            ((and (string= "GET" (lack.request:request-method ningle:*request*)) token)
                (cl-forms:set-field-value form 'ningle-auth/forms:email (ningle-auth/models:email user))
                (cl-forms:set-field-value form 'ningle-auth/forms:token (ningle-auth/models:token-value token))
                (djula:render-template* "ningle-auth/reset.html" nil :title "Create a new password" :form form))

            (t
                (handler-case
                    (progn
                        (cl-forms:handle-request form) ; Can throw an error if CSRF fails
                        (multiple-value-bind (valid errors)
                            (cl-forms:validate-form form)

                          (when errors
                            (format t "Errors: ~A~%" errors))

                          (when valid
                            (cl-forms:with-form-field-values (email token password password-verify) form
                                (when (string/= password password-verify)
                                    (error "Passwords do not match"))

                                (let* ((user (mito:find-dao 'ningle-auth/models:user :email email))
                                       (token (mito:find-dao 'ningle-auth/models:token :user user :token token :purpose ningle-auth/models:+password-reset+)))
                                  (if user
                                      (progn
                                        (setf (mito-auth:password user) password)
                                        (mito:save-dao user)
                                        (mito:delete-dao token)
                                        (ingle:redirect (concatenate 'string (get-config :mount-path) "/login")))
                                      (djula:render-template* "error.html" nil :title "Error" :error "No user found")))))))

                    (error (err)
                        (djula:render-template* "error.html" nil :title "Error" :error err))

                    (simple-error (csrf-error)
                        (setf (lack.response:response-status ningle:*response*) 403)
                        (djula:render-template* "error.html" nil :title "Error" :error csrf-error))))))))

;; Must not be fully set up
(setf (ningle:route *app* "/verify")
    (lambda (params)
      (let* ((user (mito:find-dao 'ningle-auth/models:user :username (cdr (assoc "user" params :test #'string=))))
             (token (mito:find-dao 'ningle-auth/models:token :user user :purpose ningle-auth/models:+email-verification+ :token (cdr (assoc "token" params :test #'string=)))))
        (cond
          ((and token (ningle-auth/models:is-expired-p token))
            (mito:delete-dao token)
            (let ((new-token (ningle-auth/models:generate-token user ningle-auth/models:+email-verification+)))
                (format t "Token ~A expired, issuing new token: ~A~A/verify?user=~A&token=~A~%"
                    (format nil "http://~A:~A" (lack/request:request-server-name ningle:*request*) (lack/request:request-server-port ningle:*request*))
                    (get-config :mount-path)
                    (ningle-auth/models:token-value token)
                    (ningle-auth/models:username user)
                    (ningle-auth/models:token-value new-token)))

            (djula:render-template* "ningle-auth/verify.html" nil :title "Verify" :token-reissued t))

          ((not token)
            (format t "Token ~A does not exist~%" (cdr (assoc "token" params :test #'string=)))
            (djula:render-template* "error.html" nil :title "Error" :error "Token not valid"))

          (t
            (mito:delete-dao token)
            (mito:create-dao 'ningle-auth/models:permission :user user :role (mito:find-dao 'ningle-auth/models:role :name "user"))
            (ningle-auth/models:activate user)
            (mito:save-dao user)
            (format t "User ~A activated!~%" (ningle-auth/models:username user))
            (ingle:redirect (concatenate 'string (get-config :mount-path) "/login")))))))

(defmethod ningle:not-found ((app ningle:<app>))
    (declare (ignore app))
    (setf (lack.response:response-status ningle:*response*) 404)
    (djula:render-template* "error.html" nil :title "Error" :error "Not Found"))

(defun start (&key (server :woo) (address "127.0.0.1") (port 8000))
    (djula:add-template-directory (asdf:system-relative-pathname :ningle-auth "src/templates/"))
    (djula:set-static-url "/public/")
    (clack:clackup
     (lack.builder:builder (envy-ningle:build-middleware :ningle-auth/config *app*))
     :server server
     :address address
     :port port))

(defun stop (instance)
    (clack:stop instance))
