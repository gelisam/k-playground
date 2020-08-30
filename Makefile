all: test
.PHONY: all build run test repl clean clobber

IMAGE=runtimeverificationinc/kframework-k:ubuntu-bionic-master
FLAGS=--workdir=/root --mount type=bind,source="$(shell pwd)/src",target=/root
DOCKER=docker run ${FLAGS} "${IMAGE}"

build: src/lambda-kompiled/timestamp
src/lambda-kompiled/timestamp: src/lambda.k
	@${DOCKER} kompile --backend java lambda.k &> kompile.out && (echo 0 > kompile.err) || (echo 1 > kompile.err)
	@cat kompile.out | grep -v '^\[WARNING\] Running as root is not recommended$$' || true
	@[[ "$$(cat kompile.err)" = 0 ]]

%.parsed: %.lambda src/lambda-kompiled/timestamp
	@${DOCKER} kast $(notdir $<) &> kast.out && (echo 0 > kast.err) || (echo 1 > kast.err)
	@cat kast.out | grep -v '\[WARNING\] Running as root is not recommended' > $@ || true
	@[[ "$$(cat kast.err)" = 0 ]]
	@cat $@

run: src/example.actual
%.actual: %.lambda src/lambda-kompiled/timestamp
	@${DOCKER} krun $(notdir $<) &> krun.out && (echo 0 > krun.err) || (echo 1 > krun.err)
	@cat krun.out | grep -v '\[WARNING\] Running as root is not recommended' > $@ || true
	@[[ "$$(cat krun.err)" = 0 ]]
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
	rm -rf kompile.out kompile.err kast.out kast.err krun.out krun.err src/*.parsed src/*.actual src/*.passed

clobber: clean
	rm -rf src/lambda-kompiled
