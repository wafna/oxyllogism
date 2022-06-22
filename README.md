# Oxyllogism

Fun with logic.

## Haskell

Install [GHCup](https://www.haskell.org/ghcup/).
After installing, start a new terminal as the GHCup installer doesn't update the current shell environment.

I had to install `gcc` and `make`, as well, on a fairly fresh XUbuntu.
Also required was 

    sudo apt-get install libgmp3-dev

## Building

`cabal build`

`cabal run`

Or, use the `Makefile` to stack commands, e.g.

`make clean run`