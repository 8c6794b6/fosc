;;;; Script to run test and generate code coverage report.

(push :coverage *features*)

#+ccl
(progn
  (setq ccl:*compile-code-coverage* t)
  (asdf:compile-system :fosc :force t)
  (asdf:compile-system :fosc-tests :force t)
  (asdf:test-system :fosc :force t)
  (report-coverage "/tmp/coverage-ccl-fosc/index.html"))

#+sbcl
(require :sb-cover)

#+sbcl
(progn
  (declaim (optimize (sb-cover:store-coverage-data 3)))
  (asdf:test-system :fosc :force t)
  (sb-cover:report "/tmp/coverage-sbcl-fosc/"))

(cl-user::quit)
