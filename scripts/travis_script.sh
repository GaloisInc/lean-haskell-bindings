#!/bin/bash

echo "$PATH"

export PATH="$PWD/.cabal-sandbox/bin:$PATH"
export PATH="/opt/ghc/7.10.2/bin:$PATH"

which ghc
which cabl

.cabal-sandbox/bin/cabal configure --enable-tests
.cabal-sandbox/bin/cabal build
.cabal-sandbox/bin/cabal test
