#!/bin/bash

# Configure the cabal file with a pointer to lean directories.

# This script assumes:
# * It is run from the root directory of the lean-haskell-bindings repo.
# * "deps/lean" points to the lean source.
# * "deps/lean/build/debug" contains a successful build of lean.

cabal configure --extra-lib-dirs=$PWD/deps/lean/build --extra-include-dirs=$PWD/deps/lean/src/api
