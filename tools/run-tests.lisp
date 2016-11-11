#+clisp
(progn #-asdf3 (asdf:upgrade-asdf)
       #+asdf3 (values))

#+(and clisp (not asdf))
(error "Tests requires ASDF3")

(load "~/quicklisp/setup.lisp")
(asdf:test-system :fosc)
(cl-user::quit)
