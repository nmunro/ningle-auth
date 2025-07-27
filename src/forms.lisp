(defpackage ningle-auth/forms
  (:use :cl)
  (:export #:register
           #:login
           #:reset-password
           #:new-password
           #:email
           #:username
           #:token
           #:password
           #:password-verify))

(in-package ningle-auth/forms)

(defparameter *username-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)))

(defparameter *password-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)
                                         (clavier:len :min 8)))

(cl-forms:defform register (:id "register" :csrf-protection t :csrf-field-name "csrftoken")
  ((email           :email    :value "" :constraints (list (clavier:valid-email)))
   (username        :string   :value "" :constraints *username-validator*)
   (password        :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit          :submit   :label "Register")))

(cl-forms:defform login (:id "login" :csrf-protection t :csrf-field-name "csrftoken")
  ((username :string   :value "")
   (password :password :value "")
   (submit   :submit   :value "Login")))

(cl-forms:defform reset-password (:id "password-reset" :csrf-protection 5 :csrf-field-name "csrftoken")
  ((email  :string :value "")
   (submit :submit :value "Reset")))

(cl-forms:defform new-password (:id "new-password" :csrf-protection 5 :csrf-field-name "csrftoken")
  ((email           :hidden   :value "")
   (token           :hidden   :value "")
   (password        :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit          :submit   :value "Reset")))
