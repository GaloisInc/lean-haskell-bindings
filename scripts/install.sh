#!/bin/bash

# Run cabal install with the correct options set to lean directories.
# This script assumes:
# * It is run from the root directory of the lean-haskell-bindings repo.
# * "deps/lean" points to the lean source.
# * "deps/lean/build/debug" contains a successful build of lean with the
#   shared library.

cabal install --extra-lib-dirs=$PWD/deps/lean/build/debug --extra-include-dirs=$PWD/deps/lean/src/api
