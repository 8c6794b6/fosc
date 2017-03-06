;;;; fosc-benchmarks.asd - benchmark system for fosc

;;; Workaround for taking coverage report with roswell and COVERALLS.
(if (uiop:getenv "COVERALLS")
    (defsystem :fosc-benchmarks
      :name "fosc-benchmarks")
    (defsystem :fosc-benchmarks
      :name "fosc-benchmarks"
      :depends-on (:fosc :osc :trivial-benchmark)
      :pathname "tools"
      :components ((:file "benchmarks"))
      :perform (test-op :after (o c)
                        (load-system :fosc-benchmarks)
                        (funcall (intern (string :run)
                                         '#:fosc-benchmarks)))))
