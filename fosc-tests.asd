;;;; fosc-tests.lisp - tests for fosc

(defsystem #:fosc-tests
  :name "fosc-tests"
  :depends-on (:fosc :fiveam)
  :pathname "t"
  :components ((:file "fosc"))
  :perform (test-op :after (o c)
                    (load-system :fosc-tests)
                    (funcall (intern (string :run-fosc-tests)
                                     '#:fosc-tests))))
