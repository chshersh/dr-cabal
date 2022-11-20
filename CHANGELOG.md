# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP Versioning][1]. The changelog is also
available [on GitHub][2].

[1]: https://pvp.haskell.org
[2]: https://github.com/chshersh/dr-cabal/releases

## [Unreleased]

## [0.2.0.0] — 2022-11-20 ✨

### Added

* [#28](https://github.com/chshersh/dr-cabal/issues/28):
  Implement interactive mode
  (by [@Bodigrim][Bodigrim])
* [#24](https://github.com/chshersh/dr-cabal/issues/24):
  Compute the critical path
  (by [@Bodigrim][Bodigrim])
* [#26](https://github.com/chshersh/dr-cabal/issues/26),
  [#30](https://github.com/chshersh/dr-cabal/issues/30):
  Multiple documentation improvements: suggest `--store-dir`,
  `--dependencies-only`, `mktemp`, interactive mode by default
  (by [@Bodigrim][Bodigrim], [@chshersh][chshersh])

### Removed

* Remove `watch` command. Now `dr-cabal profile` can watch and build
  the profiling output at the same time.

### Changed

* [#31](https://github.com/chshersh/dr-cabal/issues/31):
  Refactor CLI significantly:

    + Remove `watch` command
    + Use interactive mode by default (via alternate terminal screen)

  (by [@chshersh][chshersh])

## [0.1.0.0] — 2022-08-06 📚

### Added

* [#10](https://github.com/chshersh/dr-cabal/issues/10):
  Support `Haddock` phase in `cabal build` output
  (by [@diasbruno][diasbruno])
* [#14](https://github.com/chshersh/dr-cabal/issues/14):
  Enrich _Summary_ with more info: parallelism level, total dependencies summary
  (by [@bradrn][bradrn])
* Module structure refactoring to add new profiling modes easier
  (by [@diasbruno][diasbruno])

### Fixed

* Fixed type in the profile example
  (by [@tonyday567][tonyday567])

## [0.0.0.0] — 2022-07-31 🌇

Initial release prepared by [@chshersh][chshersh].

<!-- Contributors -->

[Bodigrim]: https://github.com/Bodigrim
[bradrn]: https://github.com/bradrn
[chshersh]: https://github.com/chshersh
[diasbruno]: https://github.com/diasbruno
[tonyday567]: https://github.com/tonyday567

<!-- Versions -->

[Unreleased]: https://github.com/chshersh/dr-cabal/compare/v0.2.0.0...HEAD
[0.2.0.0]: https://github.com/chshersh/dr-cabal/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/chshersh/dr-cabal/compare/v0.0.0.0...v0.1.0.0
[0.0.0.0]: https://github.com/chshersh/dr-cabal/releases/tag/v0.0.0.0
