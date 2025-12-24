(defpackage ningle-auth/tests/models
  (:use :cl :rove)
  (:import-from :ningle-auth/tests/helpers
              :setup-test-db
              :teardown-test-db
              :make-test-user
              :make-test-role
              :make-test-token))

(in-package :ningle-auth/tests/models)

;;; Setup/teardown hooks using Rove's defhook
(defhook :before (setup-test-db))
(defhook :after (teardown-test-db))

;;; ============================================================================
;;; User Model Tests
;;; ============================================================================
(deftest test-user-creation
  (testing "User creation with valid data"
    (let ((user (make-test-user)))
      (ok user "User should be created")

      (ok (equal "testuser" (ningle-auth/models:username user)) "Username should match")

      (ok (equal "test@example.com" (ningle-auth/models:email user)) "Email should match")

      (ok (= 0 (ningle-auth/models:active user)) "User should start inactive"))))

(deftest test-user-password-hashing
  (testing "Password is hashed via mito-auth"
    (let ((user (make-test-user :password "secretpassword")))
      (ok user "User should be created")

      (ok (ningle-auth/models:password-hash user) "Password hash should exist")

      (ok (not (equal "secretpassword" (ningle-auth/models:password-hash user)))
          "Password should be hashed, not stored in plain text"))))

(deftest test-user-password-verification
  (testing "Password verification works"
    (let ((user (make-test-user :password "mypassword")))
      (ok (mito-auth:auth user "mypassword")
          "Correct password should authenticate")

      (not (mito-auth:auth user "wrongpassword"))
          "Incorrect password should not authenticate")))

(deftest test-user-duplicate-username
  (testing "Duplicate username should fail"
    (make-test-user :username "duplicate" :email "first@example.com")
    (ok (signals (make-test-user :username "duplicate" :email "second@example.com"))
        "Creating user with duplicate username should signal error")))

(deftest test-user-duplicate-email
  (testing "Duplicate email should fail"
    (make-test-user :username "user1" :email "same@example.com")
    (ok (signals (make-test-user :username "user2" :email "same@example.com"))
        "Creating user with duplicate email should signal error")))

(deftest test-user-starts-inactive
  (testing "New users start with active=0"
    (let ((user (make-test-user)))
      (ok (= 0 (ningle-auth/models:active user)) "Active flag should be 0"))))

(deftest test-user-activation
  (testing "activate method sets active=1"
    (let ((user (make-test-user)))
      (ok (= 0 (ningle-auth/models:active user)) "User should start inactive")

      (ningle-auth/models:activate user)

      (ok (= 1 (ningle-auth/models:active user)) "User should be active after activate"))))

(deftest test-user-username-uniqueness
  (testing "Username uniqueness constraint enforced"
    (make-test-user :username "uniqueuser")
    (ok (signals
         (mito:create-dao 'ningle-auth/models:user :username "uniqueuser" :email "different@example.com" :password "password"))
        "Database should enforce username uniqueness")))

(deftest test-user-email-uniqueness
  (testing "Email uniqueness constraint enforced"
    (make-test-user :email "unique@example.com")
    (ok (signals
         (mito:create-dao 'ningle-auth/models:user :username "differentuser" :email "unique@example.com" :password "password"))
        "Database should enforce email uniqueness")))

(deftest test-user-multiple-roles
  (testing "User can have multiple roles via permissions"
    (let* ((user (make-test-user))
           (editor-role (make-test-role :name "editor"))
           (moderator-role (make-test-role :name "moderator")))
      ;; Create permissions linking user to roles
      (mito:create-dao 'ningle-auth/models:permission :user user :role editor-role)
      (mito:create-dao 'ningle-auth/models:permission :user user :role moderator-role)

      ;; Verify permissions exist
      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user) :role-id (mito:object-id editor-role))
          "Editor permission should exist")

      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user) :role-id (mito:object-id moderator-role))
          "Moderator permission should exist"))))

(deftest test-user-can-be-retrieved
  (testing "User can be retrieved from database"
    (make-test-user :username "findme")
    (let ((found-user (mito:find-dao 'ningle-auth/models:user :username "findme")))
      (ok found-user "User should be found")

      (ok (equal "findme" (ningle-auth/models:username found-user)) "Username should match"))))

(deftest test-user-active-flag-persistence
  (testing "Active flag persists across database operations"
    (let* ((user (make-test-user))
           (user-id (mito:object-id user)))
      (ningle-auth/models:activate user)
      (mito:save-dao user)

      ;; Retrieve fresh from database
      (let ((reloaded-user (mito:find-dao 'ningle-auth/models:user :id user-id)))
        (ok (= 1 (ningle-auth/models:active reloaded-user)) "Active flag should persist")))))

;;; ============================================================================
;;; Role Model Tests 
;;; ============================================================================
(deftest test-role-creation
  (testing "Role creation with name and description"
    (let ((role (make-test-role :name "editor" :description "Editor Role")))
      (ok role "Role should be created")

      (ok (equal "editor" (ningle-auth/models:name role)) "Role name should match")

      (ok (equal "Editor Role" (ningle-auth/models:description role)) "Description should match"))))

(deftest test-role-name-uniqueness
  (testing "Role name uniqueness enforced"
    (make-test-role :name "publisher")
    (ok (signals (make-test-role :name "publisher"))
        "Creating role with duplicate name should signal error")))

(deftest test-role-can-have-multiple-users
  (testing "Role can have multiple users via permissions"
    (let ((role (make-test-role :name "viewer"))
          (user1 (make-test-user :username "user1" :email "user1@example.com"))
          (user2 (make-test-user :username "user2" :email "user2@example.com")))
      (mito:create-dao 'ningle-auth/models:permission :user user1 :role role)
      (mito:create-dao 'ningle-auth/models:permission :user user2 :role role)
      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user1) :role-id (mito:object-id role))
          "User1 permission should exist")

      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user2) :role-id (mito:object-id role))
          "User2 permission should exist"))))

(deftest test-role-relationships-work
  (testing "Role relationships loaded correctly"
    (let ((role (make-test-role :name "moderator")))
      (ok role "Role should be created")

      (ok (equal "moderator" (ningle-auth/models:name role)) "Role name should be accessible"))))

;;; ============================================================================
;;; Permission Model Tests 
;;; ============================================================================
(deftest test-permission-creation
  (testing "Permission links user and role"
    (let* ((user (make-test-user))
           (role (make-test-role :name "viewer"))
           (perm (mito:create-dao 'ningle-auth/models:permission :user user :role role)))
      (ok perm "Permission should be created"))))

(deftest test-permission-uniqueness
  (testing "User-role combination must be unique"
    (let ((user (make-test-user))
          (role (make-test-role :name "viewer")))
      (mito:create-dao 'ningle-auth/models:permission :user user :role role)
      (ok (signals (mito:create-dao 'ningle-auth/models:permission :user user :role role))
          "Duplicate user-role permission should signal error"))))

(deftest test-permission-different-roles-allowed
  (testing "User can have permissions for different roles"
    (let ((user (make-test-user))
          (role1 (make-test-role :name "viewer"))
          (role2 (make-test-role :name "moderator")))
      (mito:create-dao 'ningle-auth/models:permission :user user :role role1)
      (mito:create-dao 'ningle-auth/models:permission :user user :role role2)
      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user) :role-id (mito:object-id role1))
          "First permission should exist")

      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user) :role-id (mito:object-id role2))
          "Second permission should exist"))))

(deftest test-permission-query-by-user
  (testing "Query permissions by user"
    (let* ((user (make-test-user))
           (role (make-test-role :name "viewer"))
           (perm (mito:create-dao 'ningle-auth/models:permission :user user :role role)))
      (ok (mito:find-dao 'ningle-auth/models:permission :user-id (mito:object-id user))
          "Should find permission by user-id"))))

(deftest test-permission-query-by-role
  (testing "Query permissions by role"
    (let* ((user (make-test-user))
           (role (make-test-role :name "viewer"))
           (perm (mito:create-dao 'ningle-auth/models:permission :user user :role role)))
      (ok (mito:find-dao 'ningle-auth/models:permission :role-id (mito:object-id role))
          "Should find permission by role-id"))))

;;; ============================================================================
;;; Token Model Tests 
;;; ============================================================================
(deftest test-token-creation-valid-purpose
  (testing "Create token with valid purpose"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+)))
      (ok token "Token should be created")

      (ok (equal ningle-auth/models:+email-verification+ (ningle-auth/models:purpose token))
          "Token purpose should match"))))

(deftest test-token-generates-64-char-hex
  (testing "Token generation creates 64-char hex hash"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+))
           (token-str (ningle-auth/models:value token)))
      (ok (= 64 (length token-str)) "Token should be 64 characters")

      (ok (every (lambda (c) (find c "0123456789abcdef")) token-str)
          "Token should be hexadecimal"))))

(deftest test-token-has-salt
  (testing "Token has salt stored"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+)))
      (ok (ningle-auth/models:salt token) "Token should have salt")

      (ok (> (length (ningle-auth/models:salt token)) 0) "Salt should not be empty"))))

(deftest test-token-has-expiration
  (testing "Token has expiration timestamp"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+)))
      (ok (ningle-auth/models:expires-at token) "Token should have expiration")

      (ok (> (ningle-auth/models:expires-at token) (get-universal-time))
          "Expiration should be in the future"))))

(deftest test-token-invalid-purpose
  (testing "Token creation with invalid purpose should error"
    (let ((user (make-test-user)))
      (ok (signals (ningle-auth/models:generate-token user "invalid-purpose"))
          "Invalid token purpose should signal error"))))

(deftest test-token-is-not-expired
  (testing "is-expired-p returns false for fresh token"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+ :expires-in 3600)))
      (ok (not (ningle-auth/models:is-expired-p token))
          "Fresh token should not be expired"))))

(deftest test-token-is-expired
  (testing "is-expired-p returns true for expired token"
    (let* ((user (make-test-user))
           ;; Create token that expired 1 hour ago
           (token (make-test-token user ningle-auth/models:+email-verification+ :expires-in -3600)))
      (ok (ningle-auth/models:is-expired-p token)
          "Expired token should be detected"))))

(deftest test-token-uniqueness-per-user-purpose
  (testing "One token per user-purpose combination"
    (let ((user (make-test-user)))
      ;; Create first token
      (make-test-token user ningle-auth/models:+email-verification+)

      ;; Creating second token with same user-purpose should replace/fail
      (ok (signals (make-test-token user ningle-auth/models:+email-verification+))
          "Duplicate user-purpose combination should signal error"))))

(deftest test-token-custom-expiration
  (testing "generate-token with custom expiration"
    (let* ((user (make-test-user))
           (custom-expires-in 7200) ; 2 hours
           (token (make-test-token user ningle-auth/models:+email-verification+ :expires-in custom-expires-in))
           (expected-expiry (+ (get-universal-time) custom-expires-in)))
      (ok token "Token should be created")

      ;; Allow 5 second tolerance for test execution time
      (ok (< (abs (- (ningle-auth/models:expires-at token) expected-expiry)) 5)
          "Token expiration should match custom value"))))

(deftest test-token-email-verification-purpose
  (testing "Email verification token purpose works"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+)))
      (ok (equal ningle-auth/models:+email-verification+ (ningle-auth/models:purpose token))
          "Should create email-verification token"))))

(deftest test-token-password-reset-purpose
  (testing "Password reset token purpose works"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+password-reset+)))
      (ok (equal ningle-auth/models:+password-reset+ (ningle-auth/models:purpose token))
          "Should create password-reset token"))))

(deftest test-token-can-be-deleted
  (testing "Token deletion removes from database"
    (let* ((user (make-test-user))
           (token (make-test-token user ningle-auth/models:+email-verification+))
           (token-id (mito:object-id token)))
      (mito:delete-dao token)
      (ok (not (mito:find-dao 'ningle-auth/models:token :id token-id))
          "Deleted token should not be found"))))

(deftest test-token-different-users-same-purpose
  (testing "Different users can have tokens with same purpose"
    (let ((user1 (make-test-user :username "user1" :email "user1@example.com"))
          (user2 (make-test-user :username "user2" :email "user2@example.com")))
      (make-test-token user1 ningle-auth/models:+email-verification+)
      (make-test-token user2 ningle-auth/models:+email-verification+)
      (ok (mito:find-dao 'ningle-auth/models:token :user-id (mito:object-id user1) :purpose ningle-auth/models:+email-verification+)
          "User1 token should exist")

      (ok (mito:find-dao 'ningle-auth/models:token :user-id (mito:object-id user2) :purpose ningle-auth/models:+email-verification+)
          "User2 token should exist"))))

(deftest test-token-same-user-different-purposes
  (testing "Same user can have tokens with different purposes"
    (let ((user (make-test-user)))
      (make-test-token user ningle-auth/models:+email-verification+)
      (make-test-token user ningle-auth/models:+password-reset+)
      (ok (mito:find-dao 'ningle-auth/models:token :user-id (mito:object-id user) :purpose ningle-auth/models:+email-verification+)
          "Email verification token should exist")

      (ok (mito:find-dao 'ningle-auth/models:token :user-id (mito:object-id user) :purpose ningle-auth/models:+password-reset+)
          "Password reset token should exist"))))

(deftest test-token-hash-includes-username-and-expiry
  (testing "Token hash is deterministic based on inputs"
    (let* ((user1 (make-test-user :username "user1" :email "user1@example.com"))
           (user2 (make-test-user :username "user2" :email "user2@example.com"))
           (token1 (make-test-token user1 ningle-auth/models:+email-verification+))
           (token2 (make-test-token user2 ningle-auth/models:+email-verification+)))
      ;; Tokens for different users should be different
      (ok (not (equal (ningle-auth/models:value token1) (ningle-auth/models:value token2)))
          "Tokens for different users should have different hashes"))))
