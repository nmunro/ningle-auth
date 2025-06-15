(defpackage ningle-auth
  (:use :cl)
  (:export #:*app*
           #:start
           #:stop))

(in-package ningle-auth)

(defvar *app* (make-instance 'ningle:app))

(djula:add-template-directory (asdf:system-relative-pathname :ningle-auth "src/templates/"))

(setf (ningle:route *app* "/register")
    (lambda (params)
        (format t "Test: ~A~%" (mito:retrieve-by-sql "SELECT 2 + 3 AS result"))
        (djula:render-template* "auth/register.html" nil :title "Register")))

(setf (ningle:route *app* "/login")
    (lambda (params)
        (djula:render-template* "auth/login.html" nil :title "Login")))

(setf (ningle:route *app* "/logout")
    (lambda (params)
        (djula:render-template* "auth/logout.html" nil :title "Logout")))

(setf (ningle:route *app* "/reset")
    (lambda (params)
        (djula:render-template* "auth/reset.html" nil :title "Reset")))

(setf (ningle:route *app* "/verify")
    (lambda (params)
        (djula:render-template* "auth/verify.html" nil :title "Verify")))

(setf (ningle:route *app* "/delete")
    (lambda (params)
        (djula:render-template* "auth/delete.html" nil :title "Delete")))

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
