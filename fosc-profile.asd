;;; fosc-profile.asd

#+sbcl
(defsystem #:fosc-profile
  :name "fosc-profile"
  :description "Profile for fosc"
  :author "8c6794b6 <8c6794b6@gmail.com>"
  :license "MIT"
  :depends-on (:fosc)
  :pathname "tools"
  :components ((:file "profile-sbcl"))
  :perform (test-op :after (o c)
                    (load-system :fosc-profile)
                    (funcall (intern (string :run) '#:fosc-profile))))
