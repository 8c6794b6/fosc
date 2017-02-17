fosc - Efficient OSC for Common Lisp
====================================

The fosc package aims to provide limited, though efficient, portable and
simple to use functions for encoding and decoding OSC messages and bundles.
The package has been tested with following implementations:

 - ABCL 1.4.0 (with Java 1.8)
 - Clozure CL 1.11-16635
 - CMUCL 21b
 - ECL 16.1.2
 - GNU CLISP 2.49
 - SBCL 1.3.14


Examples
--------

Encoding OSC message, then decoding the result:

```
> (encode-message "/foo" 100 23.4567 "eight" #(9 10 11))
===> #(47 102 111 111 0 0 0 0 44 105 102 115 98 0 0 0 0 0 0 100 65 187 167 82
101 105 103 104 116 0 0 0 0 0 0 3 9 10 11 0)

> (decode-message *)
===> ("/foo" 100 23.4567 "eight" #(9 10 11))
```

Limitations
-----------

* Does not understand floating point numbers other than `SINGLE-FLOAT` and
  `DOUBLE-FLOAT`. `RATIO` are coerced to `SINGLE-FLOAT`.

* Does support messages inside a bundle, but does not support bundles inside a
  bundle (aka. nested bundles).

* In some Common Lisp implementations, performance may worse than `cl-osc`
  package. In particular, tends to show poor performance in implementations
  without efficient string to/from octet array conversion, and without ieee
  754 floating point number to/from octet array conversion.
