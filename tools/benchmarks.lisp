;;; fosc-bench.lisp - Benchmark for fosc package.

(benchmark:define-benchmark-package :fosc-benchmarks
  (:use #:cl)
  (:export #:run))

(in-package #:fosc-benchmarks)


;;; Benchmarks

(defvar *nrepeats* 100000)

(defparameter *benchmarks* nil)

(defvar *small-message*
  (fosc:encode-message "/foo" 1 2 3.45 6.78 "9" "10"))

(defvar *small-bundle*
  (fosc:encode-bundle 12345678 '(("/foo" 1 2) ("/bar" 3.0 4.0))))

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
  (asdf:system-relative-pathname :fosc-benchmarks path))

(defparameter *datadir*
  (relative-pathname #p"data/"))

(defparameter *lisp-name*
  #+sbcl "sbcl"
  #+ccl "ccl"
  #-(or sbcl ccl) nil
  "Shortened Common Lisp implementation name.")

(defvar *max-y* 0)

(defun out-tsv ()
  "Output file for dumping benchmark result as TSV data."
  (and *lisp-name*
       (relative-pathname (format nil "data/~a.tsv" *lisp-name*))))

(eval-when (:compile-toplevel :load-toplevel)
  ;; Workaround for code coverage with cl-coverall and roswell. When
  ;; "COVERALLS" envvar is set, ignore the plottings done with gnuplot.
  (if (uiop:getenv "COVERALLS")
      (defun plot-png () nil)
      (defun plot-png ()
        (let* ((out-png (relative-pathname
                         (format nil "data/~a.png" *lisp-name*)))
               (expr
                (format
                 nil "
set terminal pngcairo enhanced font 'DejaVuSans' size 720,480;
set style data histogram;
set style histogram cluster gap 1;
set style fill solid 0.5;
set ylabel 'Real-time in seconds (lower is better)' font ',10';
set xtics scale 0 font ',8';
set ytics scale 0 font ',9';
set grid y;
set boxwidth 0.8;
set key nobox font ',9';
set yrange [0:*];
gapsize=1;
startcol=2;
endcol=3;
ncol=endcol-startcol+1;
boxwidth=1./(gapsize+ncol);
set title '~a ~a';
set output '~a';
plot for [col=2:3] '~a' using col:xticlabels(1) title columnheader,
     for [col=2:3] '~a'
     using (column(0)-1+boxwidth*(col-startcol+gapsize/2+1)-0.5):
           (column(col)+~$):col notitle
     with labels font ',9';
"
                 (lisp-implementation-type)
                 (lisp-implementation-version)
                 (namestring out-png)
                 (namestring (out-tsv))
                 (namestring (out-tsv))
                 (/ (ceiling *max-y*) 28))))
          (let ((p (uiop:launch-program (list "gnuplot" "-e" expr)
                                        :output :stream)))
            (alexandria:read-stream-content-into-string
             (uiop:process-info-output p)))))))

(defun run ()
  (let ((results-table (run-package-benchmarks :package :fosc-benchmarks
                                               :verbose t)))
    (flet ((get-total (name which)
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
            (let ((fosc (get-total name :fosc))
                  (osc (get-total name :osc)))
              (setf *max-y* (max *max-y* (max fosc osc)))
              (format tsv "~16a ~3$ ~3$~%" name fosc osc))))
        (plot-png)))))
