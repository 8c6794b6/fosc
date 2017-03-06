fosc - Efficient OSC for Common Lisp
====================================

[![Build status](https://travis-ci.org/8c6794b6/fosc.svg?branch=master)](https://travis-ci.org/8c6794b6/fosc)
[![Coverage Status](https://coveralls.io/repos/github/8c6794b6/fosc/badge.svg?branch=master)](https://coveralls.io/github/8c6794b6/fosc?branch=master)

Overview
--------

The fosc package aims to provide limited, though efficient, portable and
simple to use functions for encoding and decoding data format
in
[the Open Sound Control 1.0 Specification](http://opensoundcontrol.org/spec-1_0).

The package has been tested under Linux x86-64 with following implementations:

 - ABCL 1.4.0 (with Java 1.8)
 - Clozure CL 1.11-16635
 - CMUCL 21b
 - ECL 16.1.2
 - GNU CLISP 2.49 (requires ASDF >= 3.1)
 - SBCL 1.3.15


Examples
--------

Encoding and decoding OSC message:

```
FOSC> (encode-message "/foo" 100 23.4567 "eight" #(9 10 11))
#(47 102 111 111 0 0 0 0 44 105 102 115 98 0 0 0 0 0 0 100 65 187 167 82
  101 105 103 104 116 0 0 0 0 0 0 3 9 10 11 0)
FOSC> (decode-message *)
("/foo" 100 23.4567 "eight" #(9 10 11))
```

Encoding and decoding OSC bundle:

```
FOSC> (encode-bundle 1234 '(("/foo" 1.0 2) ("/bar" #(3 4 5) "6")))
#(35 98 117 110 100 108 101 0 0 0 0 0 0 0 4 210 0 0 0 20 47 102 111 111 0 0
  0 0 44 102 105 0 63 128 0 0 0 0 0 2 0 0 0 24 47 98 97 114 0 0 0 0 44 98
  115 0 0 0 0 3 3 4 5 0 54 0 0 0)
FOSC> (decode-bundle *)
(1234 (("/foo" 1.0 2) ("/bar" #(3 4 5) "6")))
```

Encoding and decoding recursive OSC bundle:

```
FOSC> (encode-bundle 1234 '(("/foo" 1.23 4)
                            (5678 (("/bar" #(5 6) (7 8))))
                            ("/buzz" 9.0 10.0)))
#(35 98 117 110 100 108 101 0 0 0 0 0 0 0 4 210 0 0 0 20 47 102 111 111 0 0 0
  0 44 102 105 0 63 157 112 164 0 0 0 4 0 0 0 52 35 98 117 110 100 108 101 0 0
  0 0 0 0 0 22 46 0 0 0 32 47 98 97 114 0 0 0 0 44 98 91 105 105 93 0 0 0 0 0
  2 5 6 0 0 0 0 0 7 0 0 0 8 0 0 0 20 47 98 117 122 122 0 0 0 44 102 102 0 65
  16 0 0 65 32 0 0)
FOSC> (decode-bundle *)
(1234 (("/foo" 1.23 4) (5678 (("/bar" #(5 6) (7 8)))) ("/buzz" 9.0 10.0)))
```

Benchmarks
----------

Actual performance varies across application code and environment.

Below
are
[micro benchmark](https://github.com/8c6794b6/fosc/blob/master/tools/benchmarks.lisp) results
which compares the performance of fosc and osc-20150923-git available in
quicklisp-2017-02-27, for CCL and SBCL under Linux X86-64.

![CCL](https://raw.githubusercontent.com/8c6794b6/fosc/master/images/ccl.png)

![SBCL](https://raw.githubusercontent.com/8c6794b6/fosc/master/images/sbcl.png)


Limitations
-----------

* `RATIO` values are always coerced to `SINGLE-FLOAT`.

* In some Common Lisp implementations, may show performance worse than
  `cl-osc` package. In particular, may show poor performance in
  implementations without efficient IEEE 754 floating point number to/from
  octet array conversion.
