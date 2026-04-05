(defpackage ningle-auth
  (:use :cl :sxql :ningle-auth/forms)
  (:export #:*app*
           #:setup
           #:login-required))

(in-package ningle-auth)

(defvar *app* (make-instance 'ningle:<app>))
(defparameter *user* nil)

(defun login-required (controller)
  (lambda (params)
    (if (cu-sith:logged-in-p)
        (funcall controller params)
        (ingle:redirect (format nil "~A?next=~A" (envy-ningle:get-config :login-redirect) (lack/request:request-uri ningle:*request*))))))

(defun get-user (username)
  (mito:find-dao 'ningle-auth/models:user :username username :active 1))

(defun get-permissions (user)
  (mito:select-dao 'ningle-auth/models:permission (where (:= :user_id (mito:object-id user)))))

(defun setup (&key (user 'ningle-auth/models:user) (user-p #'get-user) (default-roles '("user")) (extra-token-purposes nil))
  (setf *user* user)
  (setf ningle-auth/models:*default-roles* default-roles)
  (cu-sith:setup :user-p user-p :user-permissions #'get-permissions)
  (djula:add-template-directory (asdf:system-relative-pathname :ningle-auth "src/templates/"))

  (dolist (token extra-token-purposes)
    (apply #'ningle-auth/token-registry:register-token-purpose token)))

(setf (ningle:route *app* "/register" :method '(:GET :POST))
    (lambda (params)
        (declare (ignore params))
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
                          (when (mito:select-dao *user* (where (:or (:= :username username) (:= :email email))))
                            (error "Either username or email is already registered"))

                          (when (string/= password password-verify)
                            (error "Passwords do not match"))

                          (let ((user (mito:create-dao *user* :email email :username username :password password)))
                            (ningle-auth/models:on-register user)
                            (ingle:redirect (envy-ningle:get-config :login-redirect)))))))

                (error (err)
                    (djula:render-template* "ningle-auth/register.html" nil :form form :error (format nil "~A" err)))

                (simple-error (csrf-error)
                    (setf (lack.response:response-status ningle:*response*) 403)
                    (djula:render-template* "ningle-auth/register.html" nil :form form :error "CSRF Token Invalid")))))))

;; Must be logged out
(setf (ningle:route *app* "/login" :method '(:GET :POST))
    (lambda (params)
      (let ((form (cl-forms:find-form 'login))
            (next-url (ingle:get-param "next" params)))
          (cond
            ((cu-sith:logged-in-p)
                (ingle:redirect "/"))

            ((string= "GET" (lack.request:request-method ningle:*request*))
                (djula:render-template* "ningle-auth/login.html" nil :form form :url (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/reset")))

            (t
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
                                (ingle:redirect (or next-url (envy-ningle:get-config :login-success-redirect)))))))

                    (cu-sith:invalid-user (err)
                        (djula:render-template* "ningle-auth/login.html" nil
                                                :form form
                                                :url (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/reset")
                                                :error (format nil "~A, have you verified the account?" (cu-sith:msg err))))

                    (cu-sith:invalid-password (err)
                        (djula:render-template* "ningle-auth/login.html" nil
                                                :form form
                                                :url (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/reset")
                                                :error "Invalid Password"))

                    (simple-error (csrf-error)
                        (setf (lack.response:response-status ningle:*response*) 403)
                        (djula:render-template* "ningle-auth/login.html" nil
                                                :form form
                                                :url (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/reset")
                                                :error "CSRF Token Invalid"))))))))

;; Must be logged in
(setf (ningle:route *app* "/logout" :method :GET)
    (lambda (params)
        (declare (ignore params))
        (cu-sith:logout)
        (ingle:redirect (envy-ningle:get-config :login-redirect))))

;; Must be logged out
(setf (ningle:route *app* "/reset" :method '(:GET :POST))
    (lambda (params)
        (declare (ignore params))
        (let ((form (cl-forms:find-form 'reset-password)))
            (cond
              ((cu-sith:logged-in-p)
                (ingle:redirect (envy-ningle:get-config :login-redirect)))

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
                                (let* ((user (mito:find-dao *user* :email email))
                                       (token (mito:find-dao 'ningle-auth/models:token :user user :purpose ningle-auth/models:+password-reset+)))
                                  (cond
                                    ((and user token (not (ningle-auth/models:is-expired-p token)))
                                        (djula:render-template* "ningle-auth/reset.html" nil :form form :error "There is already a password reset in progress, either continue or wait a while before retrying"))

                                    (user
                                        (when token (mito:delete-dao token))
                                        (ningle-auth/models:on-reset user)
                                        (ingle:redirect (envy-ningle:get-config :login-redirect)))

                                    (t
                                     (ingle:redirect (envy-ningle:get-config :login-redirect)))))))))

                    (simple-error (csrf-error)
                        (setf (lack.response:response-status ningle:*response*) 403)
                        (djula:render-template* "ningle-auth/reset.html" nil :form form :error "CSRF Token Invalid"))))))))

(setf (ningle:route *app* "/reset/process" :method '(:GET :POST))
      (lambda (params)
        (let* ((form (cl-forms:find-form 'new-password))
               (user (mito:find-dao *user* :username (ingle:get-param "user" params)))
               (token (mito:find-dao 'ningle-auth/models:token :user user :purpose ningle-auth/models:+password-reset+ :token (ingle:get-param "token" params))))
          (cond
            ((cu-sith:logged-in-p)
                (ingle:redirect (envy-ningle:get-config :login-redirect)))

            ((and (string= "GET" (lack.request:request-method ningle:*request*)) (or (not token) (ningle-auth/models:is-expired-p token)))
                (ingle:redirect (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/reset")))

            ((and (string= "GET" (lack.request:request-method ningle:*request*)) token)
                (cl-forms:set-field-value form 'ningle-auth/forms:email (ningle-auth/models:email user))
                (cl-forms:set-field-value form 'ningle-auth/forms:token (ningle-auth/models:value token))
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

                                (let* ((user (mito:find-dao *user* :email email))
                                       (token (mito:find-dao 'ningle-auth/models:token :user user :token token :purpose ningle-auth/models:+password-reset+)))
                                  (if user
                                      (progn
                                        (setf (mito-auth:password user) password)
                                        (mito:save-dao user)
                                        (mito:delete-dao token)
                                        (ingle:redirect (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/login")))
                                      (ingle:redirect (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/reset")))))))

                    (error (err)
                        (djula:render-template* "ningle-auth/reset.html" nil :form form :error (format nil "~A" err)))

                    (simple-error (csrf-error)
                        (setf (lack.response:response-status ningle:*response*) 403)
                        (djula:render-template* "ningle-auth/reset.html" nil :form form :error "CSRF Token Invalid")))))))))

;; Must not be fully set up
(setf (ningle:route *app* "/verify")
    (lambda (params)
      (let* ((user (mito:find-dao *user* :username (ingle:get-param "user" params)))
             (token (mito:find-dao 'ningle-auth/models:token :user user :purpose ningle-auth/models:+email-verification+ :token (ingle:get-param "token" params))))
        (cond
          ((cu-sith:logged-in-p)
            (ingle:redirect (envy-ningle:get-config :login-redirect)))

          ((and token (ningle-auth/models:is-expired-p token))
            (mito:delete-dao token)
            (ningle-auth/models:on-register user)
            (djula:render-template* "ningle-auth/verify.html" nil :title "Verify" :token-reissued t))
            
          ((not token)
            (setf (lack.response:response-status ningle:*response*) 404)
            (djula:render-template* "ningle-auth/verify.html" nil :error "Token not valid"))

          (t
            (mito:delete-dao token)
            (ningle-auth/models:on-activate user)
            (ingle:redirect (concatenate 'string (envy-ningle:get-config :auth-mount-path) "/login")))))))
