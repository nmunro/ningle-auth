(defpackage ningle-auth/forms
  (:use :cl)
  (:export #:register
           #:login
           #:reset-password
           #:role
           #:user
           #:new-password
           #:email
           #:username
           #:token
           #:password
           #:password-verify))

(in-package ningle-auth/forms)

(defparameter *name-validator* (list (clavier:not-blank)
                                     (clavier:is-a-string)))

(defparameter *password-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)
                                         (clavier:len :min 8)))

(defparameter *token-validator* (list (clavier:not-blank)
                                      (clavier:is-a-string)
                                      (clavier:len :min 64 :max 64)))

(cl-forms:defform register (:id "register" :csrf-protection t :csrf-field-name "csrftoken")
  ((email           :email    :value "" :constraints (list (clavier:valid-email)))
   (username        :string   :value "" :constraints *name-validator*)
   (password        :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit          :submit   :label "Register")))

(cl-forms:defform login (:id "login" :csrf-protection t :csrf-field-name "csrftoken")
  ((username :string   :value "")
   (password :password :value "")
   (submit   :submit   :value "Login")))

(cl-forms:defform reset-password (:id "password-reset" :csrf-protection t :csrf-field-name "csrftoken")
  ((email  :string :value "")
   (submit :submit :value "Reset")))

(cl-forms:defform new-password (:id "new-password" :csrf-protection t :csrf-field-name "csrftoken")
  ((email           :hidden   :value "" :constraints (list (clavier:valid-email)))
   (token           :hidden   :value "" :constraints *token-validator*)
   (password        :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit          :submit   :value "Reset")))

(cl-forms:defform role (:id "role" :csrf-protection t :csrf-field-name "csrftoken")
  ((name        :string :value "" :constraints *name-validator*)
   (description :string :value "" :constraints *name-validator*)
   (submit      :submit :value "Reset")))

(cl-forms:defform user (:id "user" :csrf-protection t :csrf-field-name "csrftoken")
  ((email    :string  :value "" :constraints *name-validator*)
   (username :string  :value "" :constraints *name-validator*)
   (active   :boolean :value nil)
   (submit   :submit  :value "Reset")))
