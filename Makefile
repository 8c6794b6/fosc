.PHONY: all test

all: test

test: test-abcl test-ccl test-ccl64 test-clisp test-ecl test-sbcl

test-abcl:
	abcl --load tools/run-tests.lisp

test-ccl:
	ccl --load tools/run-tests.lisp

test-ccl64:
	ccl64 --load tools/run-tests.lisp

test-clisp:
	clisp -i tools/run-tests.lisp

test-ecl:
	ecl -shell tools/run-tests.lisp

test-sbcl:
	sbcl --script tools/run-tests.lisp

clean:
	find ./  \
	\( -name '*~' -o -name '*.fasl' -o -name '*.fas' \
	-o -name '*.lx32fsl' -o -name '*.lx64fsl' \) \
	-exec rm {} +
