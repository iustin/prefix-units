## Unreleased changes

This is a maintenance release: there are no API or behaviour changes
in the library, but the supported compiler range moves forward.

* The minimum supported compiler is now GHC 9.0 (`base >= 4.15`).
  GHC 8 is no longer tested, and the internal compatibility module
  for very old `base` versions is gone. CI now tests GHC 9.0 through
  9.14, plus the latest GHC on Linux and macOS.
* Packaging: the cabal file format is now 2.4, with an SPDX license
  name; the redundant `Setup.hs` is removed; `CHANGES.md` is
  installed as documentation; and the `source-repository this` tag
  now points at the correct release tag.
* The test suite no longer depends on `Cabal`, requires QuickCheck
  2.7 or newer, and builds without warnings on all supported
  compilers.
* Releases are now built and uploaded by a GitHub Actions workflow:
  `make release-tag` creates a signed `vX.Y.Z` tag, and pushing it
  runs a workflow that checks the tag against the cabal file and this
  changelog, builds the sdist and documentation, uploads them to
  Hackage as a candidate, and opens a draft GitHub release.
  Publishing the candidate is still a manual step by the maintainer.
  The README describes the steps.

## Version 0.3.0.1

*released Thu, 27 Apr 2023*

It turns out that even if `cabal check` shows no warnings, Hackage
doesn't accept packages with very old cabal version constraints. So
this release bumps that, which in turns makes `cabal check` actually
start to show warnings, so modernise the cabal file as a result.

There are no code changes compared to 0.3.0, so this is a no-op
release from the point of view of functionality.

## Version 0.3.0

*released Wed, 26 Apr 2023*

* Add the new quecto/ronto/ronna/quetta units. I didn't think I'd ever
  have to extend the list of units! Not sure entirely if these should
  be part of the parseKMGT function, since they're high enough to be
  out of normal computer units, but they do have lower/upper versions,
  so adding them seems the right thing.

## Version 0.2.0

*released Sun, 22 Nov 2015*

* Incompatible API change to cleanup some initial design decisions:
  the two level `FormatOption`/`FormatMode` model is removed, the
  fixed unit of `FormatOption` is moved to a new constructor
  `FormatMode`, and `FormatOption` is removed entirely. This should be
  a simpler API, at the cost of breaking compatibility.
* Fixed issue #3 (No support for negative numbers).
* Worked around issue #1 (Add 'base' unit) by adding a mode that
  disables scaling; it should have the same effect without introducing
  an artificial unit.

## Version 0.1.0.2

*released Sun, 23 Nov 2014*

* Trivial release for compatibility with QuickCheck 2.7 and older
  HUnit packages as found in Wheezy.
* The release switches the test suite to use Cabal macros, which might
  create issues in some cases.
* Fixed issue #2 (Wrong formatting for small numbers in SI mode).

## Version 0.1.0.1

*released Mon, 19 May 2014*

* Trivial release updating upper package bounds (for testing),
  updating homepage/related settings as part of move to github, and
  fixing a few documentation issues.

## Version 0.1.0

*released Thu, 03 May 2012*

* Initial release.
