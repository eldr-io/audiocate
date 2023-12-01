test:
	@cabal test

build-all:
	@cabal build

build-lib:
	@cabal build lib:audiocate

build-cli:
	@cabal build exe:audiocate

build-gui:
	@cabal build audiocate-gui

benchmark-test:
	@cabal run bench -- --svg test/output/bench_results.svg --csv test/output/bench_results.csv +RTS -T

benchmark-pattern:
	@cabal run bench -- -p $(pattern) --svg test/output/bench_results.svg --csv test/output/bench_results.csv +RTS -T

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)
