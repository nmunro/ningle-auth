(defpackage ningle-auth/config
  (:use :cl :envy))

(in-package ningle-auth/config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :ningle-auth))
    :auth-mount-path "/auth"))

(defconfig |test|
  `(:debug T
    :token-expiration 3600
    :email-backend :string
    :login-redirect "/login"
    :login-success-redirect "/"
    :middleware ((:session)
                 (:mito (:sqlite3 :database-name ,(uiop:getenv "SQLITE_DB_NAME"))))))
