;;;; fosc.asd

(defsystem #:fosc
  :name "fosc"
  :description "EFF-icient OSC"
  :author "8c6794b6 <8c6794b6@gmail.com>"
  :license "BSD"
  :version "0.2.0"
  :depends-on
  #+(or abcl allegro cmucl ccl sbcl)
  (:fast-io)
  #-(or abcl allegro cmucl ccl sbcl)
  (:fast-io :ieee-floats)
  :components ((:file "fosc"))
  :perform (test-op :after (o c)
                    (operate 'load-op :fosc-tests)
                    (operate 'test-op :fosc-tests)))

(defsystem #:fosc-tests
  :name "fosc-tests"
  :depends-on (:fosc :lisp-unit)
  :components ((:file "fosc-tests"))
  :perform (test-op :after (o c)
                    (load-system :fosc-tests)
                    (funcall (intern (string :run-fosc-tests)
                                     '#:fosc-tests))))
