;;;; fosc.asd - ASD file for main codes

(defsystem #:fosc
  :name "fosc"
  :description "Efficient OSC"
  :author "8c6794b6 <8c6794b6@gmail.com>"
  :license "MIT"
  :version "0.9.3"
  :depends-on
  #+(or abcl ccl sbcl)
  (:fast-io)
  #-(or abcl ccl sbcl)
  (:fast-io :ieee-floats)
  :pathname "src"
  :components ((:file "fosc"))
  :perform (test-op :after (o c)
                    (operate 'load-op :fosc-tests)
                    (operate 'test-op :fosc-tests)))
