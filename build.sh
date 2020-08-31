#!/bin/sh
mkdir -p .shake
stack exec -- ghc --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && \
  stack exec -- .shake/build "$@"
