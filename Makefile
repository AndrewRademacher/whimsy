boot: build

build: sandbox
	cabal build

sandbox:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	touch sandbox

clean:
	rm -f sandbox
	cabal clean
	cabal sandbox delete
