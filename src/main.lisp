(defpackage ningle-auth
  (:use :cl :sxql)
  (:import-from :ningle-auth/forms
                #:email
                #:username
                #:password
                #:password-verify
                #:register)
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

(djula:add-template-directory (asdf:system-relative-pathname :ningle-auth "src/templates/"))

(setf (ningle:route *app* "/register" :method '(:GET :POST))
    (lambda (params)
        (format t "Testin: ~A~%" (get-config :login-redirect))
        (let ((form (cl-forms:find-form 'register)))
          (setf (cl-forms::form-action form) (concatenate 'string (get-config :mount-path) "/register"))
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

                          (mito:create-dao 'ningle-auth/models:user
                                           :email email
                                           :username username
                                           :password password)
                          (ingle:redirect (get-config :login-redirect))))))

                (error (err)
                    (djula:render-template* "error.html" nil :title "Error" :error err))

                (simple-error (csrf-error)
                    (setf (lack.response:response-status ningle:*response*) 403)
                    (djula:render-template* "error.html" nil :title "Error" :error csrf-error)))))))

;; Must be logged out
(setf (ningle:route *app* "/login" :method '(:GET :POST))
    (lambda (params)
        (djula:render-template* "ningle-auth/login.html" nil :title "Login")))

;; Must be logged in
(setf (ningle:route *app* "/logout" :method '(:GET :POST))
    (lambda (params)
        (djula:render-template* "ningle-auth/logout.html" nil :title "Logout")))

;; Must be logged out
(setf (ningle:route *app* "/reset")
    (lambda (params)
        (djula:render-template* "ningle-auth/reset.html" nil :title "Reset")))

;; Must not be fully set up
(setf (ningle:route *app* "/verify")
    (lambda (params)
        (djula:render-template* "ningle-auth/verify.html" nil :title "Verify")))

;; User will have to be logged into access this route
(setf (ningle:route *app* "/delete" :method :DELETE)
    (lambda (params)
        (djula:render-template* "ningle-auth/delete.html" nil :title "Delete")))

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
