;;;; fosc.lisp -- Efficient Open Sound Control

(defpackage :fosc
  (:use #:cl #:fast-io)
  (:export #:encode-message
           #:decode-message
           #:encode-bundle
           #:decode-bundle
           #:single-float-to-bits
           #:bits-to-single-float
           #:double-float-to-bits
           #:bits-to-double-float
           #:octets-to-string
           #:string-to-octets
           #:encode-error
           #:decode-error))

(in-package #:fosc)

;; (declaim (optimize (speed 3) (safety 1)))

#-coverage
(declaim
 (inline single-float-to-bits bits-to-single-float
         double-float-to-bits bits-to-double-float
         string-to-octets octets-to-string
         encode-bundle-elt encode-message-elt encode-one-data encode-data
         encode-timetag encode-typetag encode-string
         encode-int64 encode-int32 encode-float32
         pad-always pad-when-necessary
         decode-address decode-data decode-int32 decode-float32
         decode-float64 decode-string decode-blob unpad))


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

(defconstant +int32-max+ 4294967296)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *hash-bundle*
  (make-array 8 :element-type '(unsigned-byte 8)
              :initial-contents '(35 98 117 110 100 108 101 0)))

(defvar *immediately*
  (make-array 8 :element-type '(unsigned-byte 8)
              :initial-contents '(0 0 0 0 0 0 0 1)))


;;; Implementation specific

;; ABCL and SBCL uses '(signed-byte 32) for `single-float-to-bits' and
;; `bits-to-single-float'. Others uses '(unsigned-byte 32).

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

(defun double-float-to-bits (f)
  #+abcl
  (let ((hi (system:double-float-high-bits f))
        (lo (system:double-float-low-bits f)))
    (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
  #+sbcl
  (let ((hi (sb-kernel:double-float-high-bits f))
        (lo (sb-kernel:double-float-low-bits f)))
    (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
  #+ccl
  (multiple-value-call
      #'(lambda (hi lo)
          (dpb lo (byte 32 0) (dpb hi (byte 32 32) 0)))
    (ccl::double-float-bits f))
  #-(or abcl ccl sbcl)
  (ieee-floats:encode-float64 f))

(defun bits-to-double-float (bits)
  #+abcl
  (system:make-double-float bits)
  #+sbcl
  (let ((hi (locally (declare (optimize (safety 0) (speed 3)))
              ;; XXX: No gurantee to work with different SBCL versions.
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

(defun string-to-octets (s)
  #+sbcl
  (sb-ext::string-to-octets (the simple-string s))
  #+ccl
  (ccl::encode-string-to-octets (the simple-string s))
  #+clisp
  (system::convert-string-to-bytes (the simple-string s)
                                   charset::iso-8859-1)
  #+cmucl
  (stream:string-to-octets (the simple-string s))
  #-(or sbcl ccl clisp cmucl)
  (let* ((len (length s))
         (a (make-array len :element-type '(unsigned-byte 8))))
    (loop for c across s for i from 0
       do (setf (elt a i) (char-code c))
       finally (return a))))

(defun octets-to-string (octets)
  #+sbcl
  (sb-ext:octets-to-string octets)
  #+ccl
  (ccl::decode-string-from-octets octets)
  #+clisp
  (system::convert-string-from-bytes octets charset::iso-8859-1)
  #+cmucl
  (stream::octets-to-string octets)
  #-(or sbcl ccl clisp cmucl)
  (map 'string #'code-char octets))


;;; Auxiliary

(defun pad-always (buf)
  "Pad with 0 for 1 to 4 bytes."
  (let ((n (the fixnum (mod (fast-io::output-buffer-len buf) 4))))
    (dotimes (i (- 4 n))
      (writeu8 0 buf))))

(defun pad-when-necessary (buf)
  "Pad with 0 for 0 to 3 bytes."
  (let ((n (the fixnum (mod (fast-io::output-buffer-len buf) 4))))
    (unless (eq 0 n)
      (dotimes (i (- 4 n))
        (writeu8 0 buf)))))

(defun unpad (buf)
  (let ((n (the fixnum (mod (fast-io::input-buffer-pos buf) 4))))
    (unless (eq 0 n)
      (dotimes (i (- 4 n))
        (read8 buf)))))


;;; Encoding

;;; XXX: RATIO valus are coerced to single-float in `encode-typetags' and
;;; `encode-data'.

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
        (fast-write-sequence (string-to-octets s) buf)
        (write8-be 0 buf)
        (pad-when-necessary buf))))

(defun encode-blob (buf blob)
  (declare (type (simple-array *) blob))
  (let ((len (length blob)))
    (write32-be len buf)
    (fast-write-sequence blob buf))
  (pad-when-necessary buf))

(defun encode-timetag (buf timetag)
  (cond
    ((integerp timetag) (writeu64-be timetag buf))
    ((eql :now timetag) (fast-write-sequence *immediately* buf))
    (t (encode-error "bad timetag ~a" timetag))))

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
                 (t (encode-error "unknown typetag ~a" (type-of d))))))
      (loop for d in data do (enc d) finally (pad-always buf)))))

(defun encode-one-data (buf d)
  (labels ((fn (d)
             (typecase d
               (simple-string (encode-string buf d))
               ((signed-byte 32) (encode-int32 buf d))
               (single-float (encode-float32 buf d))
               (ratio (encode-float32 buf (float d 1e0)))
               ((simple-array (unsigned-byte 8) *) (encode-blob buf d))
               (array (encode-blob buf (octets-from d)))
               ((signed-byte 64) (encode-int64 buf d))
               (double-float (encode-float64 buf d))
               (cons (dolist (e d) (fn e))))))
    (fn d)))

(defun encode-data (buf data)
  (dolist (d data) (encode-one-data buf d)))

(defun encode-message-elt (buf address data)
  (encode-one-data buf address)
  (encode-typetags buf data)
  (encode-data buf data))

(defun encode-bundle-elt (buf data)
  (fast-write-sequence
   (with-fast-output (tmp)
     (encode-message-elt tmp (car data) (cdr data))
     (encode-int32 buf (fast-io::output-buffer-len tmp)))
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
  (let ((str (octets-to-string
              (with-fast-output (out)
                (loop
                   for c = (readu8 buf)
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
  (octets-to-string
   (with-fast-output (out)
     (loop
        for c = (the (unsigned-byte 8) (readu8 buf))
        while (/= c 44)
        when (/= c 0) do (writeu8 c out)))))

(defun decode-data (buf)
  (let ((tags (loop for tag = (readu8 buf) while (not (eq tag 0))
                 collect tag)))
    (unpad buf)
    (macrolet ((tag-p (tag char)
                 `(eq ,tag ,(char-code char))))
      (labels ((dec (tags)
                 (let ((tag (car tags)))
                   (cond
                     ((tag-p tag #\i)
                      (values (decode-int32 buf) (cdr tags)))
                     ((tag-p tag #\f)
                      (values (decode-float32 buf) (cdr tags)))
                     ((tag-p tag #\s)
                      (values (decode-string buf) (cdr tags)))
                     ((tag-p tag #\b)
                      (values (decode-blob buf) (cdr tags)))
                     ((tag-p tag #\d)
                      (values (decode-float64 buf) (cdr tags)))
                     ((tag-p tag #\h)
                      (values (decode-int64 buf) (cdr tags)))
                     ((tag-p tag #\[) (fn (cdr tags) nil))
                     (t (decode-error "unknown tag ~a" tag)))))
               (fn (tags acc)
                 (cond
                   ((null tags) (nreverse acc))
                   ((tag-p (car tags) #\])
                    (values (nreverse acc) (cdr tags)))
                   (t (multiple-value-bind (elem rest) (dec tags)
                        (fn rest (cons elem acc)))))))
        (fn tags nil)))))

(defun decode-message-elt (buf)
  (cons (decode-address buf) (decode-data buf)))


;;; Main interfaces

(defun encode-message (address &rest data)
  "Encode OSC message with ADDRESS and DATA.

  (fosc::encode-message \"/foo\" 1 2)
  ===> #(47 102 111 111 0 0 0 0 44 105 105 0 0 0 0 1 0 0 0 2)
"
  (with-fast-output (buf)
    (encode-message-elt buf address data)))

(defun encode-bundle (timetag data)
  "Encode OSC bundle with TIMETAG and DATA.

TIMETAG is a NTP timestamp value. Element of the DATA should be a list of
OSC messages, which should start with an address string.

  > (encode-bundle #xffffffffffffffff '(\"/foo\" 1 2) '(\"/bar\" 4))
  ===> #(35 98 117 110 100 108 101 0 255 255 255 255 255 255 255 255 0 0 0
  20 47 102 111 111 0 0 0 0 44 105 105 0 0 0 0 1 0 0 0 2 0 0 0 16 47 98 97
  114 0 0 0 0 44 105 0 0 0 0 0 4)
"
  (with-fast-output (buf)
    (fast-write-sequence *hash-bundle* buf)
    (encode-timetag buf timetag)
    (dolist (d data)
      (encode-bundle-elt buf d))))

(defun decode-message (data)
  "Decode from octet array DATA to OSC message."
  (with-fast-input (buf data)
    (decode-message-elt buf)))

(defun decode-bundle (data)
  "Decode from octet array DATA to OSC bundle."
  (with-fast-input (buf data)
    (loop
       for expected across
         (the (simple-array (unsigned-byte 8)) *hash-bundle*)
       for u8 = (readu8 buf)
       unless (eq u8 expected)
       do (decode-error "not a bundle ~a" data))
    (let ((timetag (readu64-be buf)))
      (labels ((encode-one (acc)
                 (let ((len (handler-case (read32-be buf)
                              (end-of-file () nil))))
                   (if (and len (< 0 len))
                       (encode-one (cons (decode-message-elt buf) acc))
                       (nreverse acc)))))
        (cons timetag (encode-one nil))))))

(provide :fosc)
