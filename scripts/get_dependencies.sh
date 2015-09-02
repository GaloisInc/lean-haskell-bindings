#!/bin/bash
set -ex

########################################################################
## Cabal packages

wget https://www.stackage.org/lts/cabal.config -O - > cabal.config

if diff -u cabal.config .cabsnap/cabal.config; then
    echo "Stackage config cache hit"
    rm -rfv $HOME/.ghc;
    cp -a $HOME/.cabsnap/ghc $HOME/.ghc;
    cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabsnap/config $HOME/.cabal/;
else
    echo "Stackage config cache miss"
    rm -rf $HOME/.cabsnap;
    cabal update -v
    sed -i 's/^jobs:/-- jobs:/' $HOME/.cabal/config
    cabal install c2hs
    cabal install --only-dependencies --enable-tests
    echo "snapshotting configuration to build-cache";
    mkdir $HOME/.cabsnap;
    cp -a cabal.config $HOME/.cabsnap/config
    cp -a $HOME/.ghc $HOME/.cabsnap/ghc;
    cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin $HOME/.cabal/config $HOME/.cabsnap/;
fi

########################################################################
## Lean

if [ -d deps/lean ]; then
    pushd deps/lean > /dev/null
    git pull
    popd > /dev/null # deps/lean
else
    git clone https://github.com/leanprover/lean.git deps/lean
fi

mkdir -p deps/lean/build
pushd deps/lean/build > /dev/null
if [ ! -f Makefile ]; then
    cmake -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_CXX_COMPILER=$CXX ../src
fi
make
popd > /dev/null # deps/lean/build

########################################################################
## cabal.config

cat <<EOF >> cabal.config
extra-include-dirs: $PWD/deps/lean/src/api
extra-lib-dirs:     $PWD/deps/lean/build
EOF
