all: run
.PHONY: all run repl clean clobber

IMAGE=runtimeverificationinc/kframework-k:ubuntu-bionic-master
FLAGS=--workdir=/root --mount type=bind,source="$(shell pwd)/src",target=/root
DOCKER=docker run ${FLAGS} "${IMAGE}"

src/lambda-kompiled/timestamp:
	@${DOCKER} kompile --backend java lambda.k &> kompile.out
	@cat kompile.out | grep -v '^\[WARNING\] Running as root is not recommended$$' | grep -v '^\[Warning\] Compiler: Could not find main syntax module with name LAMBDA-SYNTAX$$' | grep -v '^in definition.  Use --syntax-module to specify one. Using LAMBDA as default.$$' || true

run: src/lambda-kompiled/timestamp
	@${DOCKER} kast example.lambda &> kast.out
	@-cat kast.out | grep -v '\[WARNING\] Running as root is not recommended' || true

repl:
	@docker run ${FLAGS} -it "${IMAGE}"  bash

clean:
	rm -rf kompile.out kast.out

clobber: clean
	rm -rf src/lambda-kompiled
