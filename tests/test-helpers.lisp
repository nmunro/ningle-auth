(defpackage ningle-auth/tests/helpers
  (:use :cl)
  (:export #:setup-test-db
           #:teardown-test-db
           #:make-test-user
           #:make-test-role
           #:make-test-token))

(in-package :ningle-auth/tests/helpers)

;;; Set environment variables for testing
(eval-when (:load-toplevel :execute)
  (setf (uiop:getenv "ENVY_CONFIG_PACKAGE") "ningle-auth/config")
  (setf (uiop:getenv "APP_ENV") "test"))

;;; ============================================================================
;;; Database Lifecycle Management
;;; ============================================================================

(defun setup-test-db ()
  "Create fresh in-memory test database with schema and seed data"
  (mito:connect-toplevel :sqlite3 :database-name ":memory:")
  (ningle-auth/migrations:migrate))

(defun teardown-test-db ()
  "Clean up test database connection"
  (handler-case
      (mito:disconnect-toplevel)

    (error (e)
      (format t "Warning: Error during test database teardown: ~A~%" e))))

;;; ============================================================================
;;; Model Factories
;;; ============================================================================

(defun make-test-user (&key (username "testuser") (email "test@example.com") (password "password123") (active 0))
  "Factory for creating test users with sensible defaults"
  (mito:create-dao 'ningle-auth/models:user :username username :email email :password password :active active))

(defun make-test-role (&key (name "test-role") (description "Test Role"))
  "Factory for creating test roles with sensible defaults"
  (mito:create-dao 'ningle-auth/models:role :name name :description description))

(defun make-test-token (user purpose &key (expires-in 3600))
  "Factory for creating test tokens

   Args:
     user - User instance to associate token with
     purpose - Token purpose string (e.g., 'email-verification', 'password-reset')
     expires-in - Expiration time in seconds (default 1 hour)

   Returns:
     Token instance"
  (ningle-auth/models:generate-token user purpose :expires-in expires-in))
