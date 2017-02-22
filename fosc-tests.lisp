;;;; fosc-tests.lisp -- Tests for fosc

(defpackage :fosc/tests
  (:use #:cl #:fosc #:fiveam)
  (:export #:run-fosc-tests))

(in-package #:fosc/tests)

;;; Auxiliary

(defun osc-equal (a b &key (test #'equal))
  "Compares OSC message A to B with TEST, returns T when equal."
  (cond
    ((and (vectorp a) (vectorp b))
     (and (eql (length a) (length b))
          (loop for x across a for y across b always (funcall test x y))))
    ((or (rationalp a) (rationalp b))
     (funcall test (coerce a 'single-float) (coerce b 'single-float)))
    ((and (atom a) (atom b))
     (funcall test a b))
    ((and (consp a) (consp b))
     (and (osc-equal (car a) (car b) :test test)
          (osc-equal (cdr a) (cdr b) :test test)))
    ((or (consp a) (consp b)) nil)
    (t (error "osc-equal: unhandled cond a=~a b=~a" a b))))

(defmacro edm (&rest datum)
  "Encode and decode OSC message with given DATUM."
  `(is (osc-equal
        (list "/foo" ,@datum)
        (decode-message (encode-message "/foo" ,@datum)))))

(defmacro edb (timetag &rest messages)
  "Encode and decode OSC bundle with given TIMETAG and MESSAGES."
  `(is (osc-equal
        '(,timetag (,@messages))
        (decode-bundle (encode-bundle ,timetag '(,@messages))))))


;;; Test suite

(def-suite fosc-suite :description "Test suite for fosc package.")

(in-suite fosc-suite)

(test simple-message
  "Tests for encoding and decoding messages with single element."
  (edm 123)
  (edm -123)
  (edm 1.234)
  (edm 1e-23)
  (edm 37/42)
  (edm 1234567.890123456789)
  (edm -9.8765)
  (edm 1.23456789d0)
  (edm 1d-31)
  (edm -123.456789d0)
  (edm -1d-31)
  (edm (ash 1 62))
  (edm (ash -1 62))
  (edm "quick")
  (edm "")
  (edm "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
")
  (edm #(1 2 3 4))
  (edm #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))
  (edm '(1 2 3)))

(test octet-vector
  (let ((addr "/octets")
        (data (make-array 8
                          :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3 4 5 6 7 8))))
    (is-true
     (osc-equal (list addr data)
                (decode-message (encode-message addr data))))))

(test mixed-message
  "Tests for encoding and decoding messages with mixed types."
  (edm 123 -1 0 1 "freq")
  (edm "freq" 1)
  (edm "foo" "" 1 "b" 2 "" #(3 4) "5" 6.78 "" "90")
  (edm #(1 2 3) 4 #(5 6 7 8 9) 10 11 12 #(13 14 15) "" #(16) "" #(17 18))
  (edm '(1 2 3) 4 '(4.5 6.7) 7.0 '("eight" "nine") "ten"
       '(12 34.567 "eight" #(9 10))))

(test conditions
  (signals (encode-error)
    (flet ((fn (x)
             (+ x 1)))
      (encode-message "/foo" (list #'fn))))
  (signals (error)
    (encode-bundle 'not-a-time-tag
                   '(("/foo" 1 2 3) ("/bar" 4 5 6))))
  (signals (decode-error)
    (let ((data (encode-message "/foo" 1 2.0)))
      ;; Manually making message with invalid OSC typetag.
      (setf (aref data 9) 255)
      (decode-message data))))

(test bundle
  "Tests for OSC bundles."
  (edb #xffffffffffffffff ("/foo" 1 2.34 "5") ("/bar" #(6 7) 8.9 0))
  (edb #xdeadbeaf12345678
       ("/foo" 1 2.34 "five" ("/bar" #(6 7) -8.9)))
  (edb #x1234567812345678
       ("/foo" 1 2.34 "5")
       ("/bar" #(6 7) 8.9 0)
       ("/buzz" "blahblahblah" 12 3.45 "" 6.7 "eight" 9)
       ("/quux" "" 12 3.45 "" 6.7d0 "eight" 9))
  (edb #xdeadbeaf ("/foo" 1 2.34 "5") ("/bar" #(6 7) (8 9 10))))

(test nested-bundle
  (let* ((data1 '(1001 (("/foo" 1 2)
                        (1002 (("/bar" 3.0 "4")
                               ("/buzz" (5 6 7))
                               (1003 (("/quux" 8)
                                      ("/qux" 9)))))
                        ("/corge" #(10 11))
                        (1004 (("/grault" 12)
                               (1005 (("/plugh" 13)))
                               ("/blah" 14))))))
         (data2 (decode-bundle (apply #'encode-bundle data1))))
    (is (osc-equal data1 data2)))
  (let* ((data1 '(1001 (("/foo" 1 2)
                        ("/bar" #(3 4))
                        (1002 (5 6 7) (8 9))
                        ("/buzz" 10))))
         (data2 (decode-bundle (apply #'encode-bundle data1))))
    (is (osc-equal data1 data2)))
  (let* ((data1 '(15877359437813939200
                  ((9 "t1" 1000 0 1 "freq" 330)
                   (9 "t1" 1001 0 1 "freq" 331 "out" 1)
                   (9 "t1" 1002 0 1 "freq" 332.0)
                   (52 1801))))
         (data2 (decode-bundle (apply #'encode-bundle data1))))
    (is (osc-equal data1 data2))))

(defun run-fosc-tests ()
  (run-all-tests))
