# Always out of date.
FORCE: ;

clean: FORCE
	@cabal clean

build: FORCE
	@cabal build

run: FORCE
	@cabal run