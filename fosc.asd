;;;; fosc.asd - ASD file for fosc package

(defsystem #:fosc
  :name "fosc"
  :description "Efficient OSC"
  :author "8c6794b6"
  :license "MIT"
  :version "0.9.7"
  :depends-on
  #+(or abcl ccl cmucl sbcl)
  (:fast-io)
  #-(or abcl ccl cmucl sbcl)
  (:fast-io :ieee-floats)
  :pathname "src"
  :components ((:file "fosc"))
  :perform (test-op :after (o c)
                    (operate 'load-op :fosc-test)
                    (operate 'test-op :fosc-test)))
