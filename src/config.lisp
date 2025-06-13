(defpackage ningle-auth/config
  (:use :cl :envy))

(in-package ningle-auth/config)

(dotenv:load-env (asdf:system-relative-pathname :ningle-auth ".env"))
(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :ningle-auth))))

(defconfig |test|
  `(:debug T
    :middleware ((:session)
                 (:mito (:sqlite3 :database-name ,(uiop:getenv "SQLITE_DB_NAME")))
                 (:static :root ,(asdf:system-relative-pathname :ningle-auth "src/static/") :path "/public/"))))
