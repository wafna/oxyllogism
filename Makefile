# Always out of date.
FORCE: ;

clean: FORCE
	@cabal v2-clean

build: FORCE
	@cabal v2-build

run: FORCE
	@cabal v2-run logic

test: FORCE
	@cabal v2-test

test-log: FORCE
	@cat ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/logic-0.1.0.0/t/test-logic/test/logic-0.1.0.0-test-logic.log


