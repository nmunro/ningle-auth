(defpackage ningle-auth/tests/routes
  (:use :cl :rove)
  (:import-from :ningle-auth/tests/helpers
              :setup-test-db
              :teardown-test-db
              :make-test-user
              :make-test-role
              :make-test-token)
  (:import-from :ningle-auth
              :*app*))

(in-package :ningle-auth/tests/routes)

;;; Setup/teardown hooks using Rove's defhook
(defhook :before (setup-test-db))
(defhook :after (teardown-test-db))

;;; ============================================================================
;;; Route Existence Tests
;;; ============================================================================
(deftest test-app-exists
  (testing "Ningle app instance exists"
    (ok ningle-auth:*app* "App should be defined")
    (ok (typep ningle-auth:*app* 'ningle:<app>)
        "App should be a Ningle app instance")))

(deftest test-register-route-registered
  (testing "/register route is registered"
    (ok (ningle:route ningle-auth:*app* "/register")
        "/register route should be registered")
    (ok (ningle:route ningle-auth:*app* "/register" :method :GET)
        "/register should accept GET")
    (ok (ningle:route ningle-auth:*app* "/register" :method :POST)
        "/register should accept POST")))

(deftest test-login-route-registered
  (testing "/login route is registered"
    (ok (ningle:route ningle-auth:*app* "/login")
        "/login route should be registered")
    (ok (ningle:route ningle-auth:*app* "/login" :method :GET)
        "/login should accept GET")
    (ok (ningle:route ningle-auth:*app* "/login" :method :POST)
        "/login should accept POST")))

(deftest test-logout-route-registered
  (testing "/logout route is registered"
    (ok (ningle:route ningle-auth:*app* "/logout")
        "/logout route should be registered")
    (ok (ningle:route ningle-auth:*app* "/logout" :method :GET)
        "/logout should accept GET")
    (ok (ningle:route ningle-auth:*app* "/logout" :method :POST)
        "/logout should accept POST")))

(deftest test-reset-route-registered
  (testing "/reset route is registered"
    (ok (ningle:route ningle-auth:*app* "/reset")
        "/reset route should be registered")
    (ok (ningle:route ningle-auth:*app* "/reset" :method :GET)
        "/reset should accept GET")
    (ok (ningle:route ningle-auth:*app* "/reset" :method :POST)
        "/reset should accept POST")))

(deftest test-reset-process-route-registered
  (testing "/reset/process route is registered"
    (ok (ningle:route ningle-auth:*app* "/reset/process")
        "/reset/process route should be registered")
    (ok (ningle:route ningle-auth:*app* "/reset/process" :method :GET)
        "/reset/process should accept GET")
    (ok (ningle:route ningle-auth:*app* "/reset/process" :method :POST)
        "/reset/process should accept POST")))

(deftest test-verify-route-registered
  (testing "/verify route is registered"
    (ok (ningle:route ningle-auth:*app* "/verify")
        "/verify route should be registered")
    (ok (ningle:route ningle-auth:*app* "/verify" :method :GET)
        "/verify should accept GET")))
