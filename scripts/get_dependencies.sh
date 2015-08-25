#!/bin/bash
set -ex

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

which clang-3.5 || echo "not found"
which clang-3.6 || echo "not found"
which clang || echo "not found"

if [ ! -f Makefile ]; then
    cmake -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_CXX_COMPILER=clang-3.6 ../src
fi

make libleanshared.dylib
popd > /dev/null # deps/lean/build
popd > /dev/null # deps/lean
popd > /dev/null # deps


if [ ! -f cabal.config ]; then
cat <<EOF > cabal.config
extra-include-dirs: $PWD/deps/lean/src/api
extra-lib-dirs:     $PWD/deps/lean/build
EOF

    wget https://www.stackage.org/lts/cabal.config -O - >> cabal.config
fi
cabal sandbox init
cabal update
cabal install c2hs
cabal install --only-dependencies
