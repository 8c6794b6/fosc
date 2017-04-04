;;;; fosc.asd - ASD file for fosc package

#-asdf3.1
(error "fosc requires ASDF >= 3.1")
#+asdf3.1
(defsystem #:fosc
  :name "fosc"
  :description "Efficient OSC"
  :author "8c6794b6"
  :license "MIT"
  :version "0.10.1"
  :class :package-inferred-system
  :depends-on
  #+(or abcl ccl sbcl)
  (:fast-io :fosc/src/main)
  #-(or abcl ccl sbcl)
  (:fast-io :ieee-floats :fosc/src/main)
  :perform (test-op :after (o c)
                    (operate 'load-op :fosc-test)
                    (operate 'test-op :fosc-test)))
