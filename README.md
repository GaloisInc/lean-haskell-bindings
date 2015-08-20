This contains Haskell bindings to the lean theorem prover.

To build this, you must build lean with shared libraries, and run `cabal install`.

You will likely need to set the `--extra-include-dirs` and `--extra-lib-dirs` flags depending
on where lean is installed.

If you have create a symlink to the lean source directory in `deps/lean`, then there are
bash scripts in the `scripts` directory that will run cabal configure and cabal install
with the flags set appropriately.  The scripts are intended primarily for development.
