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

### Derivations

The system of derivation used here requires you to provide the expected result from the application of a rule except, of course, for the introduction of premises.
This is for pedagogical purposes but it also makes the code self documenting.

Here is a short example:

```haskell
derive (neg r) $ do
    i1 <- pr $ p ⊃ (q ⊃ (neg r))
    i2 <- pr p
    i3 <- pr q
    i4 <- mp i1 i2 $ q ⊃ (neg r)
    i5 <- mp i4 i3 (neg r)
    qed i5
```

Call `derive` with the goal of the derivation, in this case `(neg r)`.
The second argument is a monadic computation over the derivation.
We add steps, recording their indices, and referencing these indices in subsequent steps.

The `pr` (premise) function introduces a premise by fiat without justification.

The `mp` (modus ponens) function takes two indices, performs modus ponens, and asserts that the result matches the expected result in the third argument.

Finally, the `qed` method asserts that the result of the indicated step matches the stated goal of the derivation.
