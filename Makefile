all: repl
IMAGE=runtimeverificationinc/kframework-k:ubuntu-bionic-master
FLAGS=--workdir=/root --mount type=bind,source="$(shell pwd)/src",target=/root

repl:
	docker run ${FLAGS} -it "${IMAGE}" bash
