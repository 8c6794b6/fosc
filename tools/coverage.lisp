;;;; Script to run test and generate code coverage report.

(push :coverage *features*)

#+ccl
(progn
  (setq ccl:*compile-code-coverage* t)
  (asdf:compile-system :fosc :force t)
  (asdf:compile-system :fosc/tests :force t)
  (asdf:test-system :fosc :force t)
  (report-coverage "/tmp/coverage-ccl-fosc/index.html"))

#+sbcl
(require :sb-cover)

#+sbcl
(progn
  (asdf:load-system :fosc)
  (unwind-protect
       (with-compilation-unit
           (:policy '(optimize (sb-cover:store-coverage-data 3)))
         (asdf:load-system :fosc :force (list :fosc))
         (sb-cover:reset-coverage)
         (asdf:test-system :fosc)
         (sb-cover:report "/tmp/coverage-sbcl-fosc/"))
    (asdf:load-system :fosc :force (list :fosc))))

(cl-user::quit)
