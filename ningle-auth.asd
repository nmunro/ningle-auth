(defsystem "ningle-auth"
  :version "0.2.0"
  :author "nmunro"
  :license "BSD3-Clause"
  :description ""
  :depends-on (:cl-dotenv
               :clack
               :djula
               :cl-forms
               :cl-forms.djula
               :cl-forms.ningle
               :envy-ningle
               :mito
               :ningle
               :ingle
               :local-time
               :cu-sith
               :ningle-email)
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "token-registry")
                 (:file "forms")
                 (:file "models" :depends-on ("token-registry"))
                 (:file "migrations")
                 (:file "main"))))
  :in-order-to ((test-op (test-op "ningle-auth/tests"))))

(defsystem "ningle-auth/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("ningle-auth"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "test-helpers")
                 (:file "test-models" :depends-on ("test-helpers"))
                 (:file "test-forms" :depends-on ("test-helpers"))
                 (:file "test-routes" :depends-on ("test-helpers")))))
  :description "Test system for ningle-auth"
  :perform (test-op (op c) (symbol-call :rove :run c)))
