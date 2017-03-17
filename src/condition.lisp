;;;; condition.lisp -- conditions used in fosc system

(defpackage :fosc/src/condition
  (:use #:cl)
  (:export #:fosc-encode-error
           #:fosc-decode-error
           #:fosc-network-error))

(in-package #:fosc/src/condition)

(macrolet ((def (name)
             `(progn
                (define-condition ,name (error)
                  ((text :initarg :text :reader text)
                   (args :initarg :args :reader args)))
                (defmethod print-object ((e ,name) stream)
                  (let* ((m (apply #'format nil (text e) (args e)))
                         (m (concatenate 'string
                                         ',(symbol-name name)
                                         ": " m)))
                    (format stream m)))
                (defun ,name (text &rest args)
                  (error ',name :text text :args args)))))
  (def fosc-encode-error)
  (def fosc-decode-error)
  (def fosc-network-error))
