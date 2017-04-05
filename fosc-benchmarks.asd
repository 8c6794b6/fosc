;;;; fosc-benchmarks.asd - benchmark system for fosc

;;; Workaround for taking coverage report with roswell and COVERALLS.
(if (uiop:getenv "COVERALLS")
    (defsystem :fosc-benchmarks
      :name "fosc-benchmarks"
      :description "Skip me when building for code coverage"
      :author "8c6794b6 <8c6794b6@gmail.com>"
      :license "MIT")
    (defsystem :fosc-benchmarks
      :name "fosc-benchmarks"
      :description "Benchmarks for fosc"
      :author "8c6794b6 <8c6794b6@gmail.com>"
      :license "MIT"
      :depends-on (:fosc :osc :trivial-benchmark)
      :pathname "tools"
      :components ((:file "benchmarks"))
      :perform (test-op :after (o c)
                        (load-system :fosc-benchmarks)
                        (funcall (intern (string :run)
                                         '#:fosc-benchmarks)))))
