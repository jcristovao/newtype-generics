# Changelog for `newtype-generics`

## [Unreleased]
- Add `Newtype` instance for `Data.Monoid.Ap`.

## [0.5.3] – 2018-03-23
- All code was moved to a new `Control.Newtype.Generics` module.
- `Control.Newtype` re-exports `Control.Newtype.Generics`, but is deprecated
  and will be removed in the next major release.

## [0.5.2.2] – 2018-03-16
- Adjust bounds for `base` and `transformers`

## [0.5.2.1] – 2018-02-16
- Reupload due to README encoding issue

## [0.5.2] – 2018-02-16
### Added
- `under2`
- `over2`

### Other
- Various documentation improvements

## [0.5.1]
### Added
- Add more instances from base

## 0.5.0.1
### Changes
- Compatibility with GHC 8.2.1

## 0.5
### Changes
- Relax types of `underF` and `overF` to allow different input & output functors


[Unreleased]: https://github.com/sjakobi/newtype-generics/compare/v0.5.3...HEAD
[0.5.3]: https://github.com/sjakobi/newtype-generics/compare/v0.5.2.2...v0.5.3
[0.5.2.2]: https://github.com/sjakobi/newtype-generics/compare/v0.5.2.1...v0.5.2.2
[0.5.2.1]: https://github.com/sjakobi/newtype-generics/compare/v0.5.2...v0.5.2.1
[0.5.2]: https://github.com/sjakobi/newtype-generics/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/sjakobi/newtype-generics/compare/v0.5.0.1...v0.5.1
