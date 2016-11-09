;;;; fosc-tests.lisp -- Tests for fosc

(defpackage :fosc-tests
  (:use #:cl #:fosc #:lisp-unit)
  (:export #:run-fosc-tests))

(in-package #:fosc-tests)

;;; Auxiliary

(defun osc-equal (a b &key (test #'equal))
  "Compares OSC message A to B with TEST, returns T when equal."
  (cond
    ((and (vectorp a) (vectorp b))
     (and (eql (length a) (length b))
          (loop for x across a for y across b always (funcall test x y))))
    ((and (atom a) (atom b))
     (funcall test a b))
    ((and (consp a) (consp b))
     (and (osc-equal (car a) (car b) :test test)
          (osc-equal (cdr a) (cdr b) :test test)))
    ((or (consp a) (consp b)) nil)
    (t (error "osc-equal: unhandled cond a=~a b=~a" a b))))

(defmacro edm (&rest datum)
  "Encode and Decode osc Message with given DATUM."
  `(assert-true
    (osc-equal '("/foo" ,@datum)
               (decode-message
                (encode-message "/foo" ,@datum)))))

(defmacro edb (timetag &rest messages)
  "Encode and Decode osc Bundle with given MESSAGES."
  `(assert-true
    (osc-equal '(,timetag ,@messages)
               (decode-bundle
                (encode-bundle ,timetag '(,@messages))))))


;;; Test suite

(define-test simple-message
  (edm 123)
  (edm -123)
  (edm 1.234)
  (edm 1e-23)
  (edm 1234567.890123456789)
  (edm 1.23456789d0)
  (edm 1d-31)
  (edm "quick")
  (edm "")
  (edm "Lorem ipsum dolor sit amet, consectetur adipiscing
elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
")
  (edm #(1 2 3 4))
  (edm #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)))

(define-test mixed-message
  (edm 123 -1 0 1 "freq")
  (edm "freq" 1)
  (edm "foo" "" 1 "b" 2 "" #(3 4) "5" 6.78 "" "90")
  (edm #(1 2 3) 4 #(5 6 7 8 9) 10 11 12 #(13 14 15) "" #(16) "" #(17 18)))

(define-test bundle
  (edb #xffffffffffffffff ("/foo" 1 2.34 "5") ("/bar" #(6 7) 8.9 0))
  (edb #x1234567812345678
       ("/foo" 1 2.34 "5")
       ("/bar" #(6 7) 8.9 0)
       ("/buzz" "blahblahblah" 12 3.45 "" 6.7 "eight" 9)
       ("/quux" "" 12 3.45 "" 6.7d0 "eight" 9)))

(defun run-fosc-tests ()
  (let ((*print-failures* t)
        (*print-errors* t))
    (run-tests :all :fosc-tests)))

(provide :fosc-tests)
