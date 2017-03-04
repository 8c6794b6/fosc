;;; fosc-profile.asd

#+sbcl
(defsystem #:fosc-profile
  :name "fosc-profile"
  :depends-on (:fosc)
  :pathname "src"
  :components ((:file "profile-sbcl"))
  :perform (test-op :after (o c)
                    (load-system :fosc-profile)
                    (funcall (intern (string :run) '#:fosc-profile))))
