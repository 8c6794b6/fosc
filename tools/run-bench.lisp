;;;; A script to run simple benchmarks.

(load "~/quicklisp/setup.lisp")

(ql:quickload 'fosc)
(ql:quickload 'osc)

(defun fosc-encode-small-message ()
  (fosc::encode-message "/foo" 1 2 3 1.0 2.0 3.0 "bar" "buzz"))

(defun osc-encode-small-message ()
  (osc::encode-message "/foo"  1 2 3 1.0 2.0 3.0 "bar" "buzz"))

(defvar *small-message-repeats* 100000)

(format t ";;; FOSC - encode-small-messages~%")
(time
 (dotimes (i *small-message-repeats*)
   (fosc-encode-small-message)))

(format t ";;; OSC - encode-small-messages~%")
(time
 (dotimes (i *small-message-repeats*)
   (osc-encode-small-message)))

(defvar *small-message-to-decode*
  (fosc::encode-message "/foo" 1 2 3 1.0 2.0 3.0 "bar" "buzz"))

(format t ";;; FOSC - decode-small-messages~%")
(time (dotimes (i *small-message-repeats*)
        (fosc::decode-message *small-message-to-decode*)))

(format t ";;; OSC - decode-small-messages~%")
(time (dotimes (i *small-message-repeats*)
        (osc::decode-message *small-message-to-decode*)))

(defun main ()
  nil)
