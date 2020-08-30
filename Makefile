all: test
.PHONY: all build run test repl clean clobber

IMAGE=runtimeverificationinc/kframework-k:ubuntu-bionic-master
FLAGS=--workdir=/root --mount type=bind,source="$(shell pwd)/src",target=/root
DOCKER=docker run ${FLAGS} "${IMAGE}"

LAMBDAKS=$(shell find src -name lambda.k)
TIMESTAMPS=$(LAMBDAKS:lambda.k=/lambda-kompiled/timestamp)

build: ${TIMESTAMPS}
%/lambda-kompiled/timestamp: %/lambda.k
	@${DOCKER} kompile --backend java $< &> kompile.out && (echo 0 > kompile.err) || (echo 1 > kompile.err)
	@cat kompile.out | grep -v '^\[WARNING\] Running as root is not recommended$$' || true
	@[[ "$$(cat kompile.err)" = 0 ]]

%.parsed: %.lambda src/lambda-kompiled/timestamp
	@${DOCKER} kast $(notdir $<) &> kast.out && (echo 0 > kast.err) || (echo 1 > kast.err)
	@cat kast.out | grep -v '^\[WARNING\] Running as root is not recommended$$' > $@ || true
	@[[ "$$(cat kast.err)" = 0 ]]
	@cat $@

run: src/operational-semantics/small-steps/example.actual
%.actual: src/examples/$(*F).lambda $(*D)/lambda-kompiled/timestamp
	echo src/examples/$(*F).lambda $(*D)/lambda-kompiled/timestamp
	${DOCKER} krun $(notdir $<) &> krun.out && (echo 0 > krun.err) || (echo 1 > krun.err)
	@cat krun.out | grep -v '^\[WARNING\] Running as root is not recommended$$' > $@ || true
	@[[ "$$(cat krun.err)" = 0 ]]
	@cat $@

EXPECTEDS=$(shell find src -name '*.expected')
ACTUALS=$(EXPECTEDS:.expected=.actual)
TESTS=$(EXPECTEDS:.expected=.passed)
.SECONDARY: ${ACTUALS}
test: ${TESTS}
	@echo "*** TESTS PASSED ***"
%.passed: %.expected %.actual
	@diff -urN $^

repl:
	@docker run ${FLAGS} -it "${IMAGE}"  bash

clean:
	rm -rf kompile.out kompile.err kast.out kast.err krun.out krun.err
	find src -name '*.parsed' | xargs rm -rf
	find src -name '*.actual' | xargs rm -rf
	find src -name '*.passed' | xargs rm -rf

clobber: clean
	rm -rf src/lambda-kompiled
