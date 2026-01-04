(defpackage ningle-auth/token-registry
  (:use :cl)
  (:export #:+token-purpose-registry+
           #:register-token-purpose
           #:token-purpose-valid-p
           #:+email-verification+
           #:+password-reset+))

(in-package :ningle-auth/token-registry)

(defparameter +email-verification+ "email-verification"
  "Token purpose for email verification during user registration")

(defparameter +password-reset+ "password-reset"
  "Token purpose for password reset requests")

(defparameter +token-purpose-registry+ (make-hash-table :test #'equal)
  "Registry of valid token purposes as strings")

(defparameter +token-purpose-predicates+ nil 
  "Registry of valid token purposes as functions")

(defun register-token-purpose (purpose &key prefix)
  "Register a token purpose as valid. Returns PURPOSE."
  (check-type purpose string)
  (check-type prefix boolean)
  (if prefix
      (push (lambda (p) (str:starts-with-p purpose p)) +token-purpose-predicates+)
      (setf (gethash purpose +token-purpose-registry+) t)))

(defun token-purpose-valid-p (purpose)
  "Check if PURPOSE is a registered token purpose."
  (or (gethash purpose +token-purpose-registry+)
      (some (lambda (pred) (funcall pred purpose)) +token-purpose-predicates+)))

;;; Register built-in token purposes at load time
(eval-when (:load-toplevel :execute)
  (register-token-purpose +email-verification+)
  (register-token-purpose +password-reset+))
