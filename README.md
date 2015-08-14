This contains Haskell bindings to the lean theorem prover.

To build this, you must build lean with shared libraries, and run `cabal install`.

You will likely need to set the `--extra-include-dirs` and `--extra-lib-dirs` flags depending
on where lean is installed.
