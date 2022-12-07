# Advent of Code 2022

Solutions to [https://adventofcode.com](https://adventofcode.com/2022).

## Development

1. [Get ghcup](https://www.haskell.org/ghcup/)
2. Install `ghc`
```sh
ghcup install ghc 9.2.5
```
3. Install `cabal-install`
```sh
ghcup install cabal
```
4. (Optional) Install ghcid and doctest
```sh
cabal install ghcid && cabal install doctest
```

### Fire up GHCi

```sh
cabal repl
```

### Or fire up the GHCi daemon

```sh
ghcid
```
This will reload the interpreter and run doctests on every file save. Requires both [ghcid](https://github.com/ndmitchell/ghcid) and [doctest](https://github.com/sol/doctest) to be on your `PATH`.
