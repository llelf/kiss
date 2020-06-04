
kiss:
	cabal v2-build --write-ghc-environment-files=always
clean:
	rm -rf dist-newstyle .ghc.environment.*
