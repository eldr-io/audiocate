# audiocate

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white) [![Haskell CI](https://github.com/eldr-io/audiocate/actions/workflows/haskell.yml/badge.svg)](https://github.com/eldr-io/audiocate/actions/workflows/haskell.yml)

Audio encoding authentication library for verifying audio as being from a trusted source

### How to
#### Compile all
```
cabal build
```

#### Run tests
```
cabal test
```

#### Run benchmarks
```
cabal run bench -- --svg test/output/bench_results.svg --csv test/output/bench_results.csv +RTS -T
```
Run benchmarks with pattern:
```
cabal run bench -- -p "stream" --svg test/output/bench_results.svg --csv test/output/bench_results.csv +RTS -T

```
