(defpackage ningle-auth/forms
  (:use :cl)
  (:export #:register
           #:email
           #:username
           #:password
           #:password-verify))

(in-package ningle-auth/forms)

(defparameter *username-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)))

(defparameter *password-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)
                                         (clavier:len :min 8)))

(cl-forms:defform register (:id "signup" :csrf-protection t :csrf-field-name "csrftoken")
  ((email           :email    :value "" :constraints (list (clavier:valid-email)))
   (username        :string   :value "" :constraints *username-validator*)
   (password        :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit          :submit   :label "Register")))
