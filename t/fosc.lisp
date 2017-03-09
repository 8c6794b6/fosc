;;;; fosc-test.lisp -- Tests for fosc

(defpackage :fosc-test
  (:use #:cl #:fosc #:fiveam)
  (:export #:run-tests))

(in-package #:fosc-test)

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

(defmacro test-edm (name &body datum)
  "Encode and decode OSC message with given DATUM."
  `(test ,name
     (is (osc-equal
          (list "/foo" ,@datum)
          (decode-message (encode-message "/foo" ,@datum))))))

(defmacro test-edb (name timetag &body messages)
  "Encode and decode OSC bundle with given TIMETAG and MESSAGES."
  `(test ,name
     (is (osc-equal
          '(,timetag (,@messages))
          (decode-bundle (encode-bundle ,timetag '(,@messages)))))))


;;; Test suite

(def-suite fosc-suite :description "Test suite for fosc package.")

(in-suite fosc-suite)

(test-edm i32-positive 123)
(test-edm i32-negative -123)
(test-edm f32-positive 1.234)
(test-edm f32-positive-small 1e-23)
(test-edm ratio 37/42)
(test-edm f32-positive-big 1234567.890123456789)
(test-edm f32-negative -9.8765)
(test-edm f64-positive 1.23456789d0)
(test-edm f64-positive-small 1d-31)
(test-edm f64-negative -123.456789d0)
(test-edm f63-negative-small -1d-31)
(test-edm i64-positive (ash 1 62))
(test-edm i64-negative (ash -1 62))
(test-edm string "quick")
(test-edm string-empty "")
(test-edm string-lorem "Lorem ipsum dolor sit amet, consectetur adipiscing
elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in
voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit
anim id est laborum.
")
(test-edm vector-4 #(1 2 3 4))
(test-edm vector-17 #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))
(test-edm list '(1 2 3))

(test octet-vector
  (let ((addr "/octets")
        (data (make-array 8
                          :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3 4 5 6 7 8))))
    (is-true
     (osc-equal (list addr data)
                (decode-message (encode-message addr data))))))

(test-edm mixed-1 123 -1 0 1 "freq")
(test-edm mixed-2 "freq" 1)
(test-edm mixed-3 "foo" "" 1 "b" 2 "" #(3 4) "5" 6.78 "" "90")
(test-edm mixed-4 #(1 2 3) 4 #(5 6 7 8 9) 10 11 12 #(13 14 15)
          "" #(16) "" #(17 18))
(test-edm mixed-5 '(1 2 3) 4 '(4.5 6.7) 7.0 '("eight" "nine") "ten"
          '(12 34.567 "eight" #(9 10)))

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

(test-edb bundle-1 #xffffffffffffffff
  ("/foo" 1 2.34 "5") ("/bar" #(6 7) 8.9 0))
(test-edb bundl-2 #xdeadbeaf12345678
  ("/foo" 1 2.34 "five" ("/bar" #(6 7) -8.9)))
(test-edb bundle-3 #x1234567812345678
  ("/foo" 1 2.34 "5")
  ("/bar" #(6 7) 8.9 0)
  ("/buzz" "blahblahblah" 12 3.45 "" 6.7 "eight" 9)
  ("/quux" "" 12 3.45 "" 6.7d0 "eight" 9))
(test-edb bundle-4 #xdeadbeaf
  ("/foo" 1 2.34 "5") ("/bar" #(6 7) (8 9 10)))

(test nested-bundle-1
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
    (is (osc-equal data1 data2))))

(test nested-bundle-2
 (let* ((data1 '(1001 (("/foo" 1 2)
                       ("/bar" #(3 4))
                       (1002 (5 6 7) (8 9))
                       ("/buzz" 10))))
        (data2 (decode-bundle (apply #'encode-bundle data1))))
   (is (osc-equal data1 data2))))

(test nested-bundle-3
 (let* ((data1 '(15877359437813939200
                 ((9 "t1" 1000 0 1 "freq" 330)
                  (9 "t1" 1001 0 1 "freq" 331 "out" 1)
                  (9 "t1" 1002 0 1 "freq" 332.0)
                  (52 1801))))
        (data2 (decode-bundle (apply #'encode-bundle data1))))
   (is (osc-equal data1 data2))))

(defun run-tests ()
  (run! 'fosc-suite))
