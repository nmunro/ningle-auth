(defsystem "ningle-auth"
  :version "0.0.1"
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
               :local-time
               :cu-sith
               :ningle-email)
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "forms")
                 (:file "models")
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
                ((:file "main"))))
  :description "Test system for ningle-auth"
  :perform (test-op (op c) (symbol-call :rove :run c)))
