;;;; network.lisp -- simple network functions with usocket

(defpackage #:fosc/src/network
  (:use #:cl
        #:fast-io
        #:usocket
        #:fosc/src/condition
        #:fosc/src/encdec)
  (:export #:open-socket
           #:close-socket
           #:send
           #:recv
           #:wait))

(in-package #:fosc/src/network)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find :coverage *features*)
    (declaim
     (inline send recv wait open-socket close-socket))))


;;; Socket

(defun open-socket (&key (host "127.0.0.1") (port 8000) (protocol :udp))
  (usocket:socket-connect host port
                          :protocol (ecase protocol
                                      (:udp :datagram)
                                      (:tcp :stream))
                          :element-type '(unsigned-byte 8)))

(defun close-socket (sock)
  (usocket:socket-close sock))


;;; Send/receive

(defun send (sock osc)
  "Send OSC data with SOCK."
  ;; Declaring `msg' as `(vector (unsigned-byte 8))'. Without the declaration,
  ;; socket code in SBCL kernel complained. Not sure whether the declaration
  ;; is necessary for other implementation.
  (let ((data (encode-osc osc)))
    (declare (type (simple-array (unsigned-byte 8)) data))
    (cond
      ((usocket:datagram-usocket-p sock)
       (usocket:socket-send sock data (length data)))
      ((usocket:stream-usocket-p sock)
       (let ((stream (usocket:socket-stream sock))
             (len (length data)))
         (write-sequence (with-fast-output (buf)
                           (writeu32-be len buf))
                         stream)
         (write-sequence data stream)
         (force-output stream)
         len))
      (t (fosc-network-error "got socket ~a" sock)))))

(defun recv (sock)
  "Receive OSC data from SOCK."
  (cond
    ((usocket:datagram-usocket-p sock)
     (multiple-value-bind (data n remote-host remote-port)
         (usocket:socket-receive sock nil 65507)
       (values (decode-osc data) n remote-host remote-port)))
    ((usocket:stream-usocket-p sock)
     (let* ((stream (usocket:socket-stream sock))
            (head32 (loop for i below 4 collect (read-byte stream)))
            (size (with-fast-input (buf (octets-from head32))
                    (readu32-be buf)))
            (buf (make-octet-vector size)))
       (read-sequence buf stream)
       (values (decode-osc buf) nil nil nil)))
    (t (fosc-network-error "got socket ~a" sock))))

(defun wait (sock tag &key (timeout 1))
  "Wait data from SOCK until OSC message starting with TAG is sent back."
  (labels ((lp ()
             (multiple-value-bind (ready rem)
                 (usocket:wait-for-input sock :timeout timeout)
               (if rem
                   (let ((msg (recv ready)))
                     (if (equal tag (car msg))
                         msg
                         (lp)))
                   (fosc-network-error "socket not readable ~a" sock)))))
    (if (stringp tag)
        (lp)
        (fosc-network-error "got non-string tag ~a" tag))))
