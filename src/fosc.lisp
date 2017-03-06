;;;; fosc.lisp -- Efficient Open Sound Control

(defpackage :fosc
  (:use #:cl #:fast-io)
  (:export #:encode-message
           #:encode-bundle
           #:encode-error
           #:decode-message
           #:decode-bundle
           #:decode-error
           #:octets-to-ascii
           #:ascii-to-octets))

(in-package #:fosc)

;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

(eval-when (:compile-toplevel)
  (unless (find :coverage *features*)
    (declaim
     (inline single-float-to-bits bits-to-single-float
             double-float-to-bits bits-to-double-float
             ascii-to-octets octets-to-ascii
             pad-always pad-when-necessary unpad hash-bundle-p

             encode-int64 encode-int32 encode-float32 encode-float64
             encode-string encode-blob encode-timetag encode-typetag
             encode-typetags encode-one-data encode-data encode-message-elt
             bundle-in-bundle-p encode-bundle-elt

             decode-int32 decode-int64 decode-float32 decode-float64
             decode-string decode-blob decode-address decode-data
             decode-message-elt decode-bundle-elts

             encode-message decode-message))))


;;; Conditions, Constants and Vars

(eval-when (:compile-toplevel)
  (defmacro define-fosc-error (name)
    `(progn
       (define-condition ,name (error)
         ((text :initarg :text :reader text)
          (args :initarg :args :reader args)))
       (defmethod print-object ((e ,name) stream)
         (let* ((m (apply #'format nil (text e) (args e)))
                (m (concatenate 'string ',(symbol-name name) ": " m)))
           (format stream m)))
       (defun ,name (text &rest args)
         (error ',name :text text :args args)))))

(define-fosc-error encode-error)
(define-fosc-error decode-error)

(defconstant +hash-bundle+
  (if (boundp '+hash-bundle+)
      (symbol-value '+hash-bundle+)
      (octets-from '(35 98 117 110 100 108 101 0))))


;;; Implementation specific: floats

;;; ABCL, CMUCL, and SBCL uses '(signed-byte 32) for `single-float-to-bits'
;;; and `bits-to-single-float'. Others uses '(unsigned-byte 32).

#+(or abcl cmucl sbcl)
(declaim
 (ftype (function (single-float) (signed-byte 32)) single-float-to-bits)
 (ftype (function ((signed-byte 32)) (single-float)) bits-to-single-float))

#-(or abcl cmucl sbcl)
(declaim
 (ftype (function (single-float) (unsigned-byte 32)) single-float-to-bits)
 (ftype (function ((unsigned-byte 32)) single-float) bits-to-single-float))

(defun single-float-to-bits (f)
  #+abcl
  (system:single-float-bits f)
  #+allegro
  (multiple-value-bind (x y)
      (excl:single-float-to-shorts f)
    (+ (ash x 16) y))
  #+ccl
  (ccl::single-float-bits f)
  #+cmucl
  (kernel:single-float-bits f)
  #+sbcl
  (sb-kernel:single-float-bits f)
  #-(or abcl allegro ccl cmucl sbcl)
  (ieee-floats:encode-float32 f))

(defun bits-to-single-float (bits)
  #+abcl
  (system:make-single-float bits)
  #+allegro
  (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                               (ldb (byte 16 0) bits))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+cmucl
  (kernel:make-single-float bits)
  #+sbcl
  (sb-kernel:make-single-float bits)
  #-(or abcl allegro ccl cmucl sbcl)
  (ieee-floats:decode-float32 bits))

(declaim
 (ftype (function (double-float) (unsigned-byte 64)) double-float-to-bits))

(defun double-float-to-bits (f)
  #+abcl
  (let ((hi (system:double-float-high-bits f))
        (lo (system:double-float-low-bits f)))
    (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
  #+ccl
  (multiple-value-call
      (lambda (hi lo)
        (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
    (ccl::double-float-bits f))
  #+cmucl
  (multiple-value-call
      (lambda (hi lo)
        (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
    (kernel:double-float-bits f))
  #+sbcl
  (let ((hi (sb-kernel:double-float-high-bits f))
        (lo (sb-kernel:double-float-low-bits f)))
    (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
  #-(or abcl ccl cmucl sbcl)
  (ieee-floats:encode-float64 f))

(declaim
 (ftype (function ((unsigned-byte 64)) double-float) bits-to-double-float))

(defun bits-to-double-float (bits)
  #+abcl
  (system:make-double-float bits)
  #+sbcl
  (let ((hi (locally (declare (optimize (safety 0) (speed 3)))
              ;; XXX: Not sure whether this works with different SBCL
              ;; versions, or different platform than x86-64 Linux.
              (the (signed-byte 32)
                   (ldb (byte 32 32) (the (unsigned-byte 64) bits)))))
        (lo (ldb (byte 32 0) bits)))
    (sb-kernel:make-double-float hi lo))
  #+ccl
  (let ((hi (ldb (byte 32 32) bits))
        (lo (ldb (byte 32 0) bits)))
    (ccl::double-float-from-bits hi lo))
  #-(or abcl ccl sbcl)
  (ieee-floats:decode-float64 bits))


;;; Implementation specific: ASCII strings

;;; Did some benchmarks with encode/decode small sized strings. Assuming
;;; that small sized strings are likely to be used as parameters in OSC
;;; messages, frequently observed than large sized strings in OSC payload
;;; data. Compared with following implementation specific functions:
;;;
;;; CCL:
;;;  - `ccl:encode-string-to-octets'
;;;  - `ccl:decode-string-from-octets'
;;;
;;; CMUCL:
;;;  - `stream:string-to-octets'
;;;  - `stream:octets-to-string'
;;;
;;; SBCL:
;;;  - `sb-ext::string-to-octets'
;;;  - `sb-ext:octets-to-string'
;;;
;;; Simple "create array, then update the contents" approach performed
;;; better than using above implementation specific code except for CMUCL's
;;; `stream:octets-to-string'. When the string is known as ASCII, manual
;;; array management performed better.

(defun ascii-to-octets (s)
  "Converts ASCII string S to octet vector."
  (declare (type simple-string s))
  #+clisp
  (system::convert-string-to-bytes s charset::iso-8859-1)
  #-(or clisp)
  (loop
     with arr = (make-octet-vector (length s))
     for c across s
     for i fixnum from 0
     do (setf (aref arr i) (char-code c))
     finally (return arr)))

(defun octets-to-ascii (data)
  "Converts octet vector DATA to ASCII string."
  (declare (type octet-vector data))
  #+clisp
  (system::convert-string-from-bytes data charset::iso-8859-1)
  #+cmucl
  (stream::octets-to-string data)
  #-(or clisp cmucl)
  (loop
     with string simple-string = (make-string (length data))
     for o across data
     for i fixnum from 0
     do (setf (aref string i) (code-char o))
     finally (return string)))


;;; Auxiliary

(defun pad-always (buf)
  "Pad with zeros for 1 to 4 bytes."
  (let ((n (the octet (mod (buffer-position buf) 4))))
    (dotimes (i (- 4 n))
      (writeu8 0 buf))))

(defun pad-when-necessary (buf)
  "Pad with zeros for 0 to 3 bytes."
  (let ((n (the octet (mod (buffer-position buf) 4))))
    (unless (eq 0 n)
      (dotimes (i (- 4 n))
        (writeu8 0 buf)))))

(defun unpad (buf)
  "Removes padded zeros from input buffer BUF."
  (let ((n (the octet (mod (buffer-position buf) 4))))
    (unless (eq 0 n)
      (dotimes (i (- 4 n))
        (read8 buf)))))

(defun hash-bundle-p (buf)
  "True if input buffer BUF starts with \"#bundle\"."
  (loop
     for expected across +hash-bundle+
     for u8 = (readu8 buf)
     always (eq u8 expected)))


;;; Encoding

;;; RATIO valus are coerced to single-float in `encode-typetags' and
;;; `encode-one-data'.

(defun encode-int64 (buf i)
  (declare (type (signed-byte 64) i))
  (write64-be i buf))

(defun encode-int32 (buf i)
  (declare (type (signed-byte 32) i))
  (write32-be i buf))

(defun encode-float32 (buf f)
  (declare (type single-float f))
  #+(or abcl cmucl sbcl)
  (write32-be (single-float-to-bits f) buf)
  #-(or abcl cmucl sbcl)
  (writeu32-be (single-float-to-bits f) buf))

(defun encode-float64 (buf f)
  (declare (type double-float f))
  (writeu64-be (double-float-to-bits f) buf))

(defun encode-string (buf s)
  (declare (type string s))
  (if (string= s "")
      (fast-write-sequence (octets-from '(0 0 0 0)) buf)
      (progn
        (fast-write-sequence (ascii-to-octets s) buf)
        (write8-be 0 buf)
        (pad-when-necessary buf))))

(defun encode-blob (buf blob)
  (declare (type octet-vector blob))
  (let ((len (length blob)))
    (write32-be len buf)
    (fast-write-sequence blob buf))
  (pad-when-necessary buf))

(defun encode-timetag (buf timetag)
  (the (unsigned-byte 64) timetag)
  (writeu64-be timetag buf))

(defun encode-typetags (buf data)
  (macrolet ((writeu8-char (char)
               `(writeu8 ,(char-code char) buf)))
    (writeu8-char #\,)
    (labels ((enc (d)
               (typecase d
                 (simple-string (writeu8-char #\s))
                 ((signed-byte 32) (writeu8-char #\i))
                 ((signed-byte 64) (writeu8-char #\h))
                 (single-float (writeu8-char #\f))
                 (double-float (writeu8-char #\d))
                 (ratio (writeu8-char #\f))
                 (array (writeu8-char #\b))
                 (cons
                  (writeu8-char #\[)
                  (dolist (e d) (enc e))
                  (writeu8-char #\]))
                 (t (encode-error "unsupported type ~a" (type-of d))))))
      (loop
         for d in data
         do (enc d)
         finally (pad-always buf)))))

(defun encode-one-data (buf d)
  (labels ((fn (d)
             (typecase d
               (simple-string (encode-string buf d))
               ((signed-byte 32) (encode-int32 buf d))
               ((signed-byte 64) (encode-int64 buf d))
               (single-float (encode-float32 buf d))
               (double-float (encode-float64 buf d))
               (ratio (encode-float32 buf (float d 1e0)))
               (octet-vector (encode-blob buf d))
               (array (encode-blob buf (octets-from d)))
               (cons (dolist (e d) (fn e))))))
    (fn d)))

(defun encode-data (buf data)
  (dolist (d data) (encode-one-data buf d)))

(defun encode-message-elt (buf address data)
  (encode-one-data buf address)
  (encode-typetags buf data)
  (encode-data buf data))

(defun bundle-in-bundle-p (data)
  ;; OSC bundle has recursive structure, which means that an element of OSC
  ;; bundle could be an OSC message or an OSC bundle.
  ;;
  ;; OSC bundle element should have unsigned-byte 64 in its CAR, and CDR is
  ;; a list of OSC elements. However, the test logic used here cannot tell
  ;; OSC message containing an integer command address and payload data
  ;; consists of nested LIST data.
  (declare (type list data))
  (and (numberp (car data))
       (consp (cadr data))
       (loop for d in (cadr data) always (consp d))))

(defun encode-bundle-elt (buf data)
  (fast-write-sequence
   (with-fast-output (tmp)
     (if (bundle-in-bundle-p data)
         (let ((encoded (encode-bundle (car data) (cadr data))))
           (fast-write-sequence encoded tmp))
         (encode-message-elt tmp (car data) (cdr data)))
     (encode-int32 buf (buffer-position tmp)))
   buf))


;;; Decoding

(defun decode-int32 (buf)
  (read32-be buf))

(defun decode-int64 (buf)
  (read64-be buf))

(defun decode-float32 (buf)
  #+(or abcl cmucl sbcl)
  (bits-to-single-float (read32-be buf))
  #-(or abcl cmucl sbcl)
  (bits-to-single-float (readu32-be buf)))

(defun decode-float64 (buf)
  (bits-to-double-float (readu64-be buf)))

(defun decode-string (buf)
  (let ((str (octets-to-ascii
              (with-fast-output (out)
                (loop
                   for c = (the octet (readu8 buf))
                   while (not (eq c 0))
                   do (writeu8 c out))))))
    (unpad buf)
    str))

(defun decode-blob (buf)
  (let* ((len (the (unsigned-byte 32) (readu32-be buf)))
         (ret (with-fast-output (out)
                (dotimes (i len)
                  (write8 (readu8 buf) out)))))
    (unpad buf)
    ret))

(defun decode-address (buf)
  ;; Some application uses an integer number instead of OSC address, in
  ;; particular, SuperCollider. Supporting such case by testing whether the
  ;; initial value is `#\/' or not. However, the intger number is limited to
  ;; 16 bit values in current logic.
  (let ((c (the octet (readu8 buf))))
    (if (eq 47 c)
        (octets-to-ascii
         (with-fast-output (out)
           (writeu8 c out)
           (loop
              for c = (the octet (readu8 buf))
              while (/= c 44)
              when (/= c 0)
              do (writeu8 c out))))
        (progn
          (readu8 buf)                  ; Ignore 1 byte.
          (prog1 (read16-be buf)        ; 16 bit value to return.
            (readu8 buf))))))           ; Character ',', ignored.

(defun decode-data (buf)
  (let ((tags (loop
                 for tag = (the octet (readu8 buf))
                 while (not (eq tag 0))
                 collect tag)))
    (unpad buf)
    (labels ((dec (tags)
               (let ((tag (car tags)))
                 (case tag
                   (#.(char-code #\i)
                      (values (decode-int32 buf) (cdr tags)))
                   (#.(char-code #\f)
                      (values (decode-float32 buf) (cdr tags)))
                   (#.(char-code #\s)
                      (values (decode-string buf) (cdr tags)))
                   (#.(char-code #\b)
                      (values (decode-blob buf) (cdr tags)))
                   (#.(char-code #\d)
                      (values (decode-float64 buf) (cdr tags)))
                   (#.(char-code #\h)
                      (values (decode-int64 buf) (cdr tags)))
                   (#.(char-code #\[) (fn (cdr tags) nil))
                   (otherwise (decode-error "unknown tag ~a" tag)))))
             (fn (tags acc)
               (cond
                 ((null tags) (nreverse acc))
                 ((eq (car tags) #.(char-code #\]))
                  (values (nreverse acc) (cdr tags)))
                 (t (multiple-value-call
                        (lambda (elem rest)
                          (fn rest (cons elem acc)))
                      (dec tags))))))
      (fn tags nil))))

(defun decode-message-elt (buf)
  (cons (decode-address buf) (decode-data buf)))

(declaim
 (ftype (function (fast-io::input-buffer) list) decode-bundle-elt))

(defun decode-bundle-elt (buf)
  (labels ((rec (acc)
             (let ((len (handler-case (read32-be buf)
                          (end-of-file () nil))))
               (if len
                   (let ((data (make-octet-vector len)))
                     (fast-read-sequence data buf 0 len)
                     (rec (cons (decode-bundle data) acc)))
                   (nreverse acc)))))
    (let ((timetag (readu64-be buf)))
      (cons timetag (list (rec nil))))))


;;; Main interfaces

(defun encode-message (address &rest data)
  "Encode OSC message with ADDRESS and DATA.

  > (encode-message \"/foo\" 1 2)
  ===> #(47 102 111 111 0 0 0 0 44 105 105 0 0 0 0 1 0 0 0 2)
"
  (with-fast-output (buf)
    (encode-message-elt buf address data)))

(defun encode-bundle (timetag data)
  "Encode OSC bundle with TIMETAG and DATA.

TIMETAG is a NTP timestamp value. DATA is a list of OSC bundle element.

  > (encode-bundle #xffffffffffffffff '((\"/foo\" 1 2) (\"/bar\" 4)))
  ===> #(35 98 117 110 100 108 101 0 255 255 255 255 255 255 255 255 0 0 0
  20 47 102 111 111 0 0 0 0 44 105 105 0 0 0 0 1 0 0 0 2 0 0 0 16 47 98 97
  114 0 0 0 0 44 105 0 0 0 0 0 4)
"
  (declare (type (unsigned-byte 64) timetag)
           (type list data))
  (with-fast-output (buf)
    (fast-write-sequence +hash-bundle+ buf)
    (encode-timetag buf timetag)
    (dolist (d data)
      (encode-bundle-elt buf d))))

(defun decode-message (data)
  "Decode from octet vector DATA to OSC message."
  (declare (type octet-vector data))
  (with-fast-input (buf data)
    (decode-message-elt buf)))

(defun decode-bundle (data)
  "Decode from octet vector DATA to OSC bundle."
  (declare (type octet-vector data))
  (with-fast-input (buf data)
    (if (hash-bundle-p buf)
        (decode-bundle-elt buf)
        (decode-message data))))
