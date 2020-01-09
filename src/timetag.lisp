;;;; timetag.lisp -- timetag for OSC bundle

(defpackage #:fosc/src/timetag
  (:use #:cl)
  (:export #:utc #:utc->ntp))

(in-package #:fosc/src/timetag)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find :coverage *features*)
    (declaim (inline utc utc->ntp)))
  #+allegro
  (progn
    (ff:def-foreign-type timeval (:struct (tv_sec :long)
                                          (tv_usec :long)))
    (ff:def-foreign-call (allegro-ffi-gettimeofday "gettimeofday")
                         ((timeval (* timeval))
                          (timezone :foreign-address))
      :returning (:int fixnum))))

(declaim
 (ftype (function () double-float) utc))

(defun utc ()
  "Returns DOUBLE-FLOAT value of seconds since 1970-01-01.
Precision varies across implementations."
  #+abcl
  (* (java:jstatic "currentTimeMillis" "java.lang.System") 1d-3)
  #+allegro
  (flet ((gettimeofday ()
           (let ((tv (ff:allocate-fobject 'timeval :c)))
             (allegro-ffi-gettimeofday tv 0)
             (let ((sec (ff:fslot-value-typed 'timeval :c tv 'tv_sec))
                   (usec (ff:fslot-value-typed 'timeval :c tv 'tv_usec)))
               (ff:free-fobject tv)
               (values sec usec)))))
    (multiple-value-bind (sec nsec) (gettimeofday)
      (+ sec (* nsec 1d-6))))
  #+(and ccl (not windows))
  (ccl:rlet ((tv :timeval))
    (ccl:external-call "gettimeofday"
                       :address tv
                       :address (ccl:%null-ptr)
                       :int)
    (+ (ccl:pref tv :timeval.tv_sec)
       (* (ccl:pref tv :timeval.tv_usec) 1d-6)))
  #+clisp
  (* (get-internal-real-time) 1d-6)
  #+cmu
  (multiple-value-bind (success-p sec nsec) (unix:unix-gettimeofday)
    (declare (ignore success-p))
    (+ sec (* nsec 1d-6)))
  #+ecl
  (progn
    (ffi:clines "#include <sys/time.h>")
    (ffi:c-inline () () :double "{
struct timeval tv;
double sec;
gettimeofday(&tv, NULL);
sec = (double) tv.tv_sec + ((double) tv.tv_usec) / 1000000;
@(return) = sec;
}"))
  #+sbcl
  (multiple-value-bind (sec nsec) (sb-ext:get-time-of-day)
    (the double-float (+ sec (* nsec 1d-6))))
  #-(or abcl (and ccl (not windows)) clisp cmu ecl sbcl)
  ;; CL's get-universal-time uses an epoch of 1/1/1900. Adjust the result to
  ;; the Unix epoch.
  (let ((unix-epoch #.(encode-universal-time 0 0 0 1 1 1970 0)))
    (coerce (- (get-universal-time) unix-epoch) 'double-float)))

(defun utc->ntp (utc-time)
  (declare (double-float utc-time))
  (multiple-value-bind (sec nsec)
      (truncate utc-time 1d0)
    (declare (type (unsigned-byte 32) sec)
             (type double-float nsec))
    ;; The `unix-epoch' is pre-computed to support ABCL. The value
    ;; `2208988800' was obtained by evaluating:
    ;;
    ;;   (encode-universal-time 0 0 0 1 1 1970 0)
    ;;
    ;; in other Common Lisp implementations.
    (let* ((unix-epoch 2208988800)
           (int32-max #.(expt 2 32))
           (high-bits (the (unsigned-byte 32) (+ sec unix-epoch)))
           (a (the (unsigned-byte 64) (ash high-bits 32)))
           (b (the (unsigned-byte 64) (round (* nsec int32-max)))))
      (the (unsigned-byte 64) (+ a b)))))
