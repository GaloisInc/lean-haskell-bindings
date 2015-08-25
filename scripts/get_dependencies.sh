#!/bin/bash
set -ex

########################################################################
## Cabal packages

if [ ! -f cabal.config ]; then
    wget https://www.stackage.org/lts/cabal.config -O - > cabal.config
fi

ls -l /opt/ghc/7.10.2

export PATH="$PWD/.cabal-sandbox/bin:$PATH"
export PATH="/opt/ghc/7.10.2/bin:$PATH"
which ghc

cabal sandbox init
cabal update

ls -la .
ls -l .cabal-sandbox || echo "sandbox not found"
ls -l .cabal-sandbox/bin || echo "not found"

type cabal
type alex

cabal install -v cabal-install
CABAL=$(which cabal)

type cabal
cabal --version
$CABAL --version

$CABAL install alex
$CABAL install happy

cabal install c2hs
cabal install --only-dependencies --enable-tests

########################################################################
## Lean

mkdir -p deps

pushd deps > /dev/null

if [ -d lean ]; then
    pushd lean > /dev/null
    git pull
else
    git clone https://github.com/leanprover/lean.git
    pushd lean > /dev/null
fi

mkdir -p build
pushd build > /dev/null

if [ ! -f Makefile ]; then
    cmake -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_CXX_COMPILER=g++-4.9 ../src
fi

make leanshared || cat Makefile
popd > /dev/null # deps/lean/build
popd > /dev/null # deps/lean
popd > /dev/null # deps

########################################################################
## cabal.config

cat <<EOF >> cabal.config
extra-include-dirs: $PWD/deps/lean/src/api
extra-lib-dirs:     $PWD/deps/lean/build
EOF
