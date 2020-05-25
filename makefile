
kiss:
	cabal v2-build --write-ghc-environment-files=always
gen:
	alex L.x && happy P.y && gsed --in-place -e 's/-w/-Wno-overlapping-patterns/' P.hs
clean:
	rm -rf L.hs P.hs dist-newstyle .ghc.environment.*
