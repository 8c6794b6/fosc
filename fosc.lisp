;;;; fosc.lisp -- EFF-icient Open Sound Control

(defpackage :fosc
  (:use #:cl #:fast-io)
  (:export #:encode-message
           #:encode-bundle
           #:decode-message
           #:decode-bundle))

(in-package #:fosc)

;; (declaim (optimize (speed 3) (safety 1)))

(declaim
 (inline encode-bundle-elt encode-message-elt encode-data
         encode-timetag encode-typetag encode-string
         encode-int64 encode-int32 encode-float32 pad
         decode-address decode-data decode-int32 decode-float32
         decode-string decode-blob octets-to-string unpad))

;;; Auxiliary

(defun pad (buf)
  (dotimes (i (the fixnum (- 4 (mod (fast-io::output-buffer-len buf) 4))))
    (writeu8 0 buf)))

(defun unpad (buf)
  (let ((n (mod (fast-io::input-buffer-pos buf) 4)))
    (unless (eq 0 n)
      (dotimes (i (- 4 n))
        (read8 buf)))))


;;; Conditions, Constants and Vars

(define-condition fosc-encode-error (error)
  ((data :initarg :data :reader data)))

(define-condition fosc-decode-error (error)
  ((data :initarg :data :reader data)))

(defconstant +int32-max+ 4294967296)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *hash-bundle*
  (make-array 8 :element-type '(unsigned-byte 8)
              :initial-contents '(35 98 117 110 100 108 101 0)))

(defvar *immediately*
  (make-array 8 :element-type '(unsigned-byte 8)
              :initial-contents '(0 0 0 0 0 0 0 1)))


;;; Encoding

;;; XXX: DOUBLE-FLOAT and RATIOR are coerced to single-float in
;;; `encode-typetags' and `encode-data'.

(defun encode-int64 (buf i)
  (declare (type (signed-byte 64) i))
  (write64-be i buf))

(defun encode-int32 (buf i)
  (declare (type (signed-byte 32) i))
  (write32-be i buf))

(defun encode-float32 (buf f)
  #+abcl
  (encode-int32 buf (system:single-float-bits f))
  #+allegro
  (encode-int32 buf (multiple-value-bind (x y)
                        (excl:single-float-to-shorts f)
                      (+ (ash x 16) y)))
  #+ccl
  (encode-int32 buf (CCL::SINGLE-FLOAT-BITS f))
  #+cmucl
  (encode-int32 buf (kernel:single-float-bits f))
  #+sbcl
  (encode-int32 buf (sb-kernel:single-float-bits f))
  #-(or abcl allegro ccl cmucl sbcl)
  (encode-int32 buf (ieee-floats:encode-float32 f)))

(defun encode-string (buf s)
  (fast-write-sequence
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
        finally (return a)))
   buf)
  (pad buf))

(defun encode-blob (buf blob)
  (declare (type (simple-array *) blob))
  (write32-be (length blob) buf)
  (fast-write-sequence blob buf)
  (pad buf))

(defun encode-timetag (buf timetag)
  (cond
    ((integerp timetag) (writeu64-be timetag buf))
    ((eql :now timetag) (fast-write-sequence *immediately* buf))
    (t (error 'fosc-encode-error :data timetag))))

(defun encode-typetags (buf data)
  (macrolet ((writeu8-char (char)
               `(writeu8 ,(char-code char) buf)))
    (writeu8-char #\,)
    (loop for d in data
       do (typecase d
            (simple-string (writeu8-char #\s))
            ((signed-byte 32) (writeu8-char #\i))
            ((signed-byte 64) (writeu8-char #\h))
            (single-float (writeu8-char #\f))
            (double-float (writeu8-char #\f))
            (ratio (writeu8-char #\f))
            (array (writeu8-char #\b))
            (t (error 'fosc-encode-error :data d)))
       finally (pad buf))))

(defun encode-data (buf data)
  (loop for d in data
     do (typecase d
          (simple-string (encode-string buf d))
          ((signed-byte 32) (encode-int32 buf d))
          ((signed-byte 64) (encode-int64 buf d))
          (single-float (encode-float32 buf d))
          (double-float (encode-float32 buf (float d 1e0)))
          (ratio (encode-float32 buf (float d 1e0)))
          ((simple-array (unsigned-byte 8) *) (encode-blob buf d))
          (array (encode-blob buf (octets-from d)))
          (t (error 'fosc-encode-error :data d)))))

(defun encode-message-elt (buf address data)
  (encode-string buf address)
  (encode-typetags buf data)
  (encode-data buf data))

(defun encode-bundle-elt (buf data)
  (fast-write-sequence
   (octets-from (with-fast-output (tmp)
                  (encode-message-elt tmp (first data) (rest data))
                  (encode-int32 buf (fast-io::output-buffer-len tmp))))
   buf))


;;; Decoding

(defun decode-int32 (buf)
  (read32-be buf))

(defun decode-float32 (buf)
  #+abcl
  (system:make-single-float (decode-int32 buf))
  #+allegro
  (excl:shorts-to-single-float (ldb (byte 16 16) (decode-int32 buf))
                               (ldb (byte 16 0) (decode-int32 buf)))
  #+ccl
  (CCL::HOST-SINGLE-FLOAT-FROM-UNSIGNED-BYTE-32 (decode-int32 buf))
  #+cmucl
  (kernel:make-single-float (decode-int32 buf))
  #+sbcl
  (sb-kernel:make-single-float (decode-int32 buf))
  #-(or abcl allegro ccl cmucl sbcl)
  (ieee-floats:decode-float32 (decode-int32 buf)))

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

(defun decode-string (buf)
  (let ((str (octets-to-string
              (with-fast-output (out)
                (loop for c = (readu8 buf)
                   while (not (eq c 0)) do (writeu8 c out))))))
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
    (loop
       for tag in tags
       for d = (macrolet ((tag-p (char)
                            `(eq tag ,(char-code char))))
                 (cond
                   ((tag-p #\i) (decode-int32 buf))
                   ((tag-p #\f) (decode-float32 buf))
                   ((tag-p #\s) (decode-string buf))
                   ((tag-p #\b) (decode-blob buf))
                   (t (error 'fosc-decode-error :data tag))))
       collect d)))

(defun decode-message-elt (buf)
  (cons (decode-address buf) (decode-data buf)))


;;; Exported

(defun encode-message (address &rest data)
  "Encode OSC message with ADDRESS and DATA.

  (fosc::encode-message \"/foo\" 1 2)
  ===> #(47 102 111 111 0 0 0 0 44 105 105 0 0 0 0 1 0 0 0 2)
"
  (with-fast-output (buf)
    (encode-message-elt buf address data)))

(defun encode-bundle (timetag &rest data)
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
       do (error 'fosc-decode-error :data data))
    (let ((timetag (readu64-be buf)))
      (labels ((encode-one (acc)
                 (let ((len (handler-case (read32-be buf)
                              (end-of-file () nil))))
                   (if (and len (< 0 len))
                       (encode-one (cons (decode-message-elt buf) acc))
                       (nreverse acc)))))
        (cons timetag (encode-one nil))))))

(provide :fosc)
