(defpackage ningle-auth/token-registry
  (:use :cl)
  (:export #:+token-purposes+
           #:register-token-purpose
           #:token-purpose-valid-p
           #:list-token-purposes
           #:+email-verification+
           #:+password-reset+))

(in-package :ningle-auth/token-registry)

(defparameter +email-verification+ "email-verification"
  "Token purpose for email verification during user registration")

(defparameter +password-reset+ "password-reset"
  "Token purpose for password reset requests")

(defparameter +token-purposes+ (make-hash-table :test #'equal)
  "Registry of valid token purposes")

(defun register-token-purpose (purpose)
  "Register a token purpose as valid. Returns PURPOSE."
  (check-type purpose string)
  (setf (gethash purpose +token-purposes+) t)
  purpose)

(defun token-purpose-valid-p (purpose)
  "Check if PURPOSE is a registered token purpose."
  (and (stringp purpose) (gethash purpose +token-purposes+)))

(defun list-token-purposes ()
  "Return a list of all registered token purposes."
  (loop for key being the hash-keys of +token-purposes+ collect key))

;;; Register built-in token purposes at load time
(eval-when (:load-toplevel :execute)
  (register-token-purpose +email-verification+)
  (register-token-purpose +password-reset+))
