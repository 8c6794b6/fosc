;;; fosc-bench.lisp - Benchmark for fosc package.

(benchmark:define-benchmark-package :fosc/benchmarks
  (:use :cl)
  (:export :run))

(in-package :fosc/benchmarks)


;;; Benchmarks

(defvar *nrepeats* 100000)

(defparameter *benchmarks* nil)

(defmacro bench (name fosc-expr osc-expr)
  (labels ((make-bench-name (which)
             (intern (string-upcase (format nil "~a-~a" name which))))
           (make-benchmark (name expr)
             `(define-benchmark ,name ()
                  (loop
                     :repeat *nrepeats*
                     :do (with-benchmark-sampling
                           ,expr)))))
    (let ((fosc-name (make-bench-name 'fosc))
          (osc-name (make-bench-name 'osc)))
      `(progn
         (push (list ',name :fosc ',fosc-name :osc ',osc-name)
               *benchmarks*)
         ,(make-benchmark fosc-name fosc-expr)
         ,(make-benchmark osc-name osc-expr)))))

(defvar *small-message*
  (fosc:encode-message "/foo" 1 2 3.45 6.78 "9" "10"))

(defvar *small-bundle*
  (fosc:encode-bundle 12345678 '(("/foo" 1 2) ("/bar" 3.0 4.0))))

(bench encode-message
  (fosc:encode-message "/foo" 1 2 3.45 6.78 "9" "10")
  (osc:encode-message "/foo" 1 2 3.45 6.78 "9" "10"))

(bench encode-bundle
  (fosc:encode-bundle 1 '(("/foo" 1 2) ("/bar" 3.0 4.0)))
  (osc:encode-bundle '(("/foo" 1 2) ("/bar" 3.0 4.0))))

(bench decode-message
  (fosc:decode-message *small-message*)
  (osc:decode-message *small-message*))

(bench decode-bundle
  (fosc:decode-bundle *small-bundle*)
  (osc:decode-bundle *small-bundle*))


;;; Running and plotting

(defun relative-pathname (path)
  (asdf:system-relative-pathname :fosc/benchmarks path))

(defparameter *datadir*
  (relative-pathname #p"data/"))

(defparameter *lisp-name*
  #+sbcl "sbcl"
  #+ccl "ccl"
  #+cmucl "cmucl"
  #-(or sbcl ccl cmucl) nil
  "Shortened Common Lisp implementation name.")

(defun out-tsv ()
  "Output file for dumping benchmark result as TSV data."
  (and *lisp-name*
       (relative-pathname (format nil "data/~a.tsv" *lisp-name*))))

(defun plot-png ()
  (let* ((out-png (relative-pathname
                   (format nil "data/~a.png" *lisp-name*)))
         (expr
          (format
           nil "
set terminal pngcairo enhanced font 'DejaVuSans' size 720,480;
set style data histogram;
set style histogram cluster gap 1;
set style fill solid 0.5 noborder;
set ylabel 'Real-time in seconds (lower is better)' font ',10';
set xtics scale 0 font ',8';
set ytics scale 0 font ',9';
set grid y;
set boxwidth 0.8;
set key nobox font ',9';
set yrange [0:*];
set title '~a ~a';
set output '~a';
plot for [col=2:3] '~a' using col:xticlabels(1) title columnheader;
"
           (lisp-implementation-type)
           (lisp-implementation-version)
           (namestring out-png)
           (namestring (out-tsv)))))
    (let ((p (uiop:launch-program (list "gnuplot" "-e" expr)
                                  :output :stream)))
      (alexandria:read-stream-content-into-string
       (uiop:process-info-output p)))))

(defun run ()
  (let ((results-table (run-package-benchmarks :package :fosc/benchmarks
                                               :verbose t)))
    (flet ((get-total (which name)
             (let* ((names (find name *benchmarks* :key #'car))
                    (bench-name (getf (cdr names) which))
                    (results (gethash bench-name results-table))
                    (real (find 'real-time results :key #'car)))
               (getf (cdr real) :total))))
      (when (out-tsv)
        (ensure-directories-exist *datadir*)
        (with-open-file (tsv (out-tsv)
                             :direction :output
                             :if-exists :supersede)
          (format tsv "benchmark fosc osc~%")
          (dolist (name (nreverse (mapcar #'car *benchmarks*)))
            (let ((fosc (get-total :fosc name))
                  (osc (get-total :osc name)))
              (format tsv "~16a ~3$ ~3$~%" name fosc osc))))
        (plot-png)))))
