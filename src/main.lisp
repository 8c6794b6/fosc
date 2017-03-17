;;;; main.lisp -- interfacing package for outside world

(in-package #:cl-user)

(uiop:define-package #:fosc/src/main
  (:nicknames #:fosc)
  (:use #:cl
        #:fosc/src/condition
        #:fosc/src/timetag
        #:fosc/src/encdec
        #:fosc/src/network)
  (:reexport #:fosc/src/condition
             #:fosc/src/timetag
             #:fosc/src/encdec
             #:fosc/src/network))
