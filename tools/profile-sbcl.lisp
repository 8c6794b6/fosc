;;;; fosc-profile.lisp - SBCL specific profiling for fosc code.

(defpackage :fosc-profile
  (:use #:cl #:fosc)
  (:export #:run))

(in-package #:fosc-profile)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defvar *bundle1*
  (encode-bundle 12345678 '(("/foo" 1 2) ("/bar" 3.4 5.6))))

(defun decode-bundle-n (data n)
  (dotimes (i (expt 2 n))
    (decode-bundle data)))

(sb-sprof:profile-call-counts "CL-USER" "FAST-IO" "FOSC" "FOSC-PROFILE")

(defun run ()
  (sb-sprof:with-profiling (:max-samples 10000
                                         :report :flat
                                         :loop nil)
    (decode-bundle-n *bundle1* 20)))
