(defpackage ningle-auth/tests/main
  (:use :cl
        :ningle-auth
        :rove))
(in-package :ningle-auth/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :ningle-auth)` in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))
