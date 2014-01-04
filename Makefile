all:
	cabal install && .hsenv/cabal/bin/dominion
repo:
	new_bitbucket_repo dominion
spec:
	cabal install && .hsenv/cabal/bin/dominion-spec
