all: test
.PHONY: all build run test repl clean clobber

IMAGE=runtimeverificationinc/kframework-k:ubuntu-bionic-master
FLAGS=--workdir=/root --mount type=bind,source="$(shell pwd)/src",target=/root
DOCKER=docker run ${FLAGS} "${IMAGE}"

build: src/lambda-kompiled/timestamp
src/lambda-kompiled/timestamp: src/lambda.k
	@${DOCKER} kompile --backend java lambda.k &> kompile.out
	@cat kompile.out | grep -v '^\[WARNING\] Running as root is not recommended$$' | grep -v '^\[Warning\] Compiler: Could not find main syntax module with name LAMBDA-SYNTAX$$' | grep -v '^in definition.  Use --syntax-module to specify one. Using LAMBDA as default.$$' || true

%.parsed: %.lambda src/lambda-kompiled/timestamp
	@${DOCKER} kast $(notdir $<) &> kast.out
	@-cat kast.out | grep -v '\[WARNING\] Running as root is not recommended' > $@ || true
	@cat $@

run: src/example.actual
%.actual: %.lambda src/lambda-kompiled/timestamp
	@${DOCKER} krun $(notdir $<) &> krun.out
	@-cat krun.out | grep -v '\[WARNING\] Running as root is not recommended' > $@ || true
	@cat $@

LAMBDAS=$(wildcard src/*.lambda)
ACTUALS=$(LAMBDAS:.lambda=.actual)
TESTS=$(LAMBDAS:.lambda=.passed)
.SECONDARY: ${ACTUALS}
test: ${TESTS}
	@echo "*** TESTS PASSED ***"
%.passed: %.expected %.actual
	@diff -urN $^

repl:
	@docker run ${FLAGS} -it "${IMAGE}"  bash

clean:
	rm -rf kompile.out kast.out krun.out src/*.parsed src/*.actual src/*.passed

clobber: clean
	rm -rf src/lambda-kompiled
