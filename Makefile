# Handy shortcuts.

# Always out of date.
FORCE: ;

clean: FORCE
	@cabal v2-clean

build: FORCE
	@cabal v2-build

test: FORCE
	@cabal v2-test

test-log: FORCE
	@less ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/Oxyllogism-0.3.0.0/t/test-logic/test/Oxyllogism-0.3.0.0-test-logic.log

sandbox: FORCE
	@cabal v2-run sandbox

monad-demo: FORCE
	@cabal v2-run monad-demo

docco: FORCE
	@cabal v2-haddock sandbox
