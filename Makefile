.PHONY: all test

all: test

test: test-abcl test-ccl test-cmucl test-ecl test-sbcl

test-abcl:
	ros -L abcl tools/run-test.ros

test-ccl:
	ros -L ccl-bin tools/run-test.ros

# test-clisp:
# 	clisp -i tools/run-test.ros

test-cmucl:
	ros -L cmu-bin tools/run-test.ros

test-ecl:
	ros -L ecl tools/run-test.ros

test-sbcl:
	ros -L sbcl tools/run-test.ros

coverage: coverage-ccl coverage-sbcl

coverage-ccl:
	ros -L ccl-bin tools/coverage.lisp

coverage-sbcl:
	ros -L sbcl tools/coverage.lisp

clean:
	find ./  \
	\( -name '*~' -o -name '*.fasl' -o -name '*.fas' \
	-o -name '*.lx32fsl' -o -name '*.lx64fsl' \) \
	-exec rm {} +
