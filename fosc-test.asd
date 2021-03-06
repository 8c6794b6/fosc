;;;; fosc-test.lisp - tests for fosc

(defsystem #:fosc-test
  :name "fosc-test"
  :description "Tests for fosc"
  :author "8c6794b6"
  :license "MIT"
  :depends-on (:fosc
               :bordeaux-threads
               :fiveam
               :usocket)
  :pathname "t"
  :components ((:file "fosc"))
  :perform (test-op :after (o c)
                    (load-system :fosc-test)
                    (symbol-call :fosc-test :run-tests)))
