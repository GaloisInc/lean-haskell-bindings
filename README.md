<table>
  <tr>
    <th>License</th><th>Ubuntu</th>
  </tr>
  <tr>
    <td><a href="LICENSE"><img src="https://img.shields.io/badge/license-APACHE_2-green.svg?dummy" title="License"/></a></td>
    <td><a href="https://travis-ci.org/GaloisInc/lean-haskell-bindings"><img src="https://travis-ci.org/GaloisInc/lean-haskell-bindings.svg?branch=master" title="Ubuntu 12.04 LTS 64bit, g++-4.9"/></a></td>
  </tr>
</table>

This contains Haskell bindings to the lean theorem prover.

To build this, you must build lean with shared libraries, and run `cabal install`.

You will likely need to set the `--extra-include-dirs` and `--extra-lib-dirs` flags depending
on where lean is installed.

If you have create a symlink to the lean source directory in `deps/lean`, then there are
bash scripts in the `scripts` directory that will run cabal configure and cabal install
with the flags set appropriately.  The scripts are intended primarily for development.
