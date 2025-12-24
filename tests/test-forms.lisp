(defpackage ningle-auth/tests/forms
  (:use :cl :rove)
  (:import-from :ningle-auth/forms
                :register
                :login
                :reset-password
                :new-password))

(in-package :ningle-auth/tests/forms)

;;; ============================================================================
;;; Register Form Tests
;;; ============================================================================

(deftest test-register-form-exists
  (testing "Register form is defined"
    (ok (cl-forms:find-form 'ningle-auth/forms:register)
        "Register form should be defined")))

(deftest test-register-form-has-required-fields
  (testing "Register form has all required fields"
    (let ((form (cl-forms:get-form 'ningle-auth/forms:register)))
      (ok (cl-forms:get-field form 'ningle-auth/forms:email)
          "Form should have email field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:username)
          "Form should have username field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:password)
          "Form should have password field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:password-verify)
          "Form should have password-verify field"))))

;;; ============================================================================
;;; Login Form Tests
;;; ============================================================================

(deftest test-login-form-exists
  (testing "Login form is defined"
    (ok (cl-forms:find-form 'ningle-auth/forms:login)
        "Login form should be defined")))

(deftest test-login-form-has-required-fields
  (testing "Login form has username and password fields"
    (let ((form (cl-forms:get-form 'ningle-auth/forms:login)))
      (ok (cl-forms:get-field form 'ningle-auth/forms:username)
          "Form should have username field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:password)
          "Form should have password field"))))

;;; ============================================================================
;;; Reset Password Form Tests
;;; ============================================================================

(deftest test-reset-password-form-exists
  (testing "Reset password form is defined"
    (ok (cl-forms:find-form 'ningle-auth/forms:reset-password)
        "Reset password form should be defined")))

(deftest test-reset-password-form-has-email-field
  (testing "Reset password form has email field"
    (let ((form (cl-forms:get-form 'ningle-auth/forms:reset-password)))
      (ok (cl-forms:get-field form 'ningle-auth/forms:email)
          "Email field should exist"))))

;;; ============================================================================
;;; New Password Form Tests
;;; ============================================================================

(deftest test-new-password-form-exists
  (testing "New password form is defined"
    (ok (cl-forms:find-form 'ningle-auth/forms:new-password)
        "New password form should be defined")))

(deftest test-new-password-form-has-required-fields
  (testing "New password form has all required fields"
    (let ((form (cl-forms:get-form 'ningle-auth/forms:new-password)))
      (ok (cl-forms:get-field form 'ningle-auth/forms:email)
          "Form should have email field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:token)
          "Form should have token field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:password)
          "Form should have password field")

      (ok (cl-forms:get-field form 'ningle-auth/forms:password-verify)
          "Form should have password-verify field"))))
