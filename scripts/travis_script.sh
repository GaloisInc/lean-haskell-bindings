#!/bin/bash

export PATH="$PWD/.cabal-sandbox/bin:$PATH"
export PATH="/opt/ghc/7.10.2/bin:$PATH"

.cabal-sandbox/bin/cabal configure --enable-tests
.cabal-sandbox/bin/cabal build
LD_LIBRARY_PATH="$PWD/deps/lean/build" ./cabal-sandbox/bin/cabal test
