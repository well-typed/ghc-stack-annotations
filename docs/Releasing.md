# Release notes

* [ ]: Update README, verify tests succeed.
* [ ]: Bump version in `ghc-stack-annotations.cabal` in a [PVP compatible way](https://pvp.haskell.org/).
* [ ]: Create `release/<version>` branch.
* [ ]: Create PR and wait for CI to succeed.
* [ ]: Merge PR
* [ ]: `git tag <version>`
* [ ]: `git push --tags`
* [ ]: Upload to hackage
  * `cabal sdist`
  * `cabal upload dist-newstyle/sdist/ghc-stack-annotations-<version>.tar.gz`
