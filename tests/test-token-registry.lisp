(defpackage ningle-auth/tests/token-registry
  (:use :cl :rove)
  (:import-from :ningle-auth/token-registry
                #:+email-verification+
                #:+password-reset+
                #:+token-purpose-registry+
                #:register-token-purpose
                #:token-purpose-valid-p))

(in-package :ningle-auth/tests/token-registry)

;;; ============================================================================
;;; Built-in Token Purpose Tests
;;; ============================================================================

(deftest test-builtin-purposes-registered
  (testing "Built-in token purposes are registered at load time"
    (ok (token-purpose-valid-p +email-verification+)
        "Email verification purpose should be registered")

    (ok (token-purpose-valid-p +password-reset+)
        "Password reset purpose should be registered")))

(deftest test-builtin-constants
  (testing "Built-in token purpose constants have correct values"
    (ok (string= "email-verification" +email-verification+)
        "Email verification constant should be 'email-verification'")

    (ok (string= "password-reset" +password-reset+)
        "Password reset constant should be 'password-reset'")))

;;; ============================================================================
;;; Custom Token Purpose Registration Tests
;;; ============================================================================

(deftest test-register-custom-purpose
  (testing "Register a custom token purpose"
    ;; Clear any previous test registrations
    (clrhash +token-purpose-registry+)
    (register-token-purpose +email-verification+)
    (register-token-purpose +password-reset+)

    ;; Register new custom purpose
    (register-token-purpose "two-factor-auth")

    (ok (token-purpose-valid-p "two-factor-auth")
        "Custom token purpose should be registered")))

(deftest test-register-multiple-purposes
  (testing "Register multiple custom token purposes"
    ;; Clear registry
    (clrhash +token-purpose-registry+)
    (register-token-purpose +email-verification+)
    (register-token-purpose +password-reset+)

    ;; Register multiple purposes
    (register-token-purpose "magic-link")
    (register-token-purpose "account-deletion")
    (register-token-purpose "phone-verification")

    (ok (token-purpose-valid-p "magic-link")
        "Magic link purpose should be valid")

    (ok (token-purpose-valid-p "account-deletion")
        "Account deletion purpose should be valid")

    (ok (token-purpose-valid-p "phone-verification")
        "Phone verification purpose should be valid")))

(deftest test-invalid-purpose
  (testing "Unregistered token purpose should be invalid"
    (ok (not (token-purpose-valid-p "nonexistent-purpose"))
        "Unregistered purpose should return NIL")

    (ok (not (token-purpose-valid-p ""))
        "Empty string should be invalid")

    (ok (not (token-purpose-valid-p "random-unregistered-type"))
        "Random unregistered type should be invalid")))

;;; ============================================================================
;;; Prefix-based Token Purpose Tests
;;; ============================================================================

(deftest test-prefix-token-purpose
  (testing "Register token purpose with prefix matching"
    ;; Clear and re-register
    (clrhash +token-purpose-registry+)
    (setf ningle-auth/token-registry::+token-purpose-predicates+ nil)
    (register-token-purpose +email-verification+)
    (register-token-purpose +password-reset+)

    ;; Register a prefix-based purpose
    (register-token-purpose "api-key-" :prefix t)

    ;; These should all match because they start with "api-key-"
    (ok (token-purpose-valid-p "api-key-read")
        "Should match 'api-key-read' with prefix")

    (ok (token-purpose-valid-p "api-key-write")
        "Should match 'api-key-write' with prefix")

    (ok (token-purpose-valid-p "api-key-admin")
        "Should match 'api-key-admin' with prefix")

    ;; These should NOT match
    (ok (not (token-purpose-valid-p "api-token-read"))
        "Should not match different prefix")

    (ok (not (token-purpose-valid-p "read-api-key"))
        "Should not match suffix")))

(deftest test-multiple-prefix-purposes
  (testing "Multiple prefix-based purposes can coexist"
    ;; Clear and re-register
    (clrhash +token-purpose-registry+)
    (setf ningle-auth/token-registry::+token-purpose-predicates+ nil)
    (register-token-purpose +email-verification+)
    (register-token-purpose +password-reset+)

    ;; Register multiple prefix purposes
    (register-token-purpose "oauth-" :prefix t)
    (register-token-purpose "jwt-" :prefix t)

    ;; Test oauth- prefix
    (ok (token-purpose-valid-p "oauth-google")
        "Should match oauth-google")

    (ok (token-purpose-valid-p "oauth-github")
        "Should match oauth-github")

    ;; Test jwt- prefix
    (ok (token-purpose-valid-p "jwt-access")
        "Should match jwt-access")

    (ok (token-purpose-valid-p "jwt-refresh")
        "Should match jwt-refresh")

    ;; Test that prefixes work correctly
    (ok (token-purpose-valid-p "jwt-google")
        "jwt-google should be valid under jwt- prefix")

    (ok (not (token-purpose-valid-p "google-jwt"))
        "google-jwt should not match (wrong order)")))

;;; ============================================================================
;;; Type Checking Tests
;;; ============================================================================

(deftest test-register-purpose-type-checking
  (testing "register-token-purpose validates input types"
    (ok (handler-case
            (progn
              (register-token-purpose 123)
              nil)
          (type-error () t))
        "Should raise type-error for non-string purpose")

    (ok (handler-case
            (progn
              (register-token-purpose "valid" :prefix "not-boolean")
              nil)
          (type-error () t))
        "Should raise type-error for non-boolean prefix")))
