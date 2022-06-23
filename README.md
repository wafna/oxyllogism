# Oxyllogism

A system for constructing, evaluating, and transforming statements in propositional logic.

## Haskell

Before installing Haskell, check to see if `gcc` and `make` exist.
You may also need to install some missing libs as follows:


    sudo apt-get install libgmp3-dev
    sudo apt-get install libtinfo-dev

Install [GHCup](https://www.haskell.org/ghcup/).
This will install the Haskell compiler (ghc) and the build system (cabal).

After installing, start a new terminal as the GHCup installer doesn't update the current shell environment.

Now, install some dependencies.

    cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit

If you get errors you may be missing some dependent libraries (see above).

## Building

Use `cabal` directly:

    cabal build
    cabal run

Or, use the `Makefile` to stack commands:

    make clean run
    make clean test test-log

## Using

Modify `src/main/Main.hs`, then `make run`.
