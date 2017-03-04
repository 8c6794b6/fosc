;;;; fosc-benchmarks.asd - benchmark system for fosc

(defsystem :fosc-benchmarks
  :name "fosc-benchmarks"
  :depends-on (:fosc :osc :trivial-benchmark)
  :pathname "src"
  :components ((:file "benchmarks"))
  :perform (test-op :after (o c)
                    (load-system :fosc-benchmarks)
                    (funcall (intern (string :run) '#:fosc-benchmarks))))
