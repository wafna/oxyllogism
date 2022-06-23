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