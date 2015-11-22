Version 0.2.0

* Incompatible API change to cleanup some initial design decisions:
  the two level `FormatOption`/`FormatMode` model is removed, the
  fixed unit of `FormatOption` is moved to a new constructor
  `FormatMode`, and `FormatOption` is removed entirely. This should be
  a simpler API, at the cost of breaking compatibility.
* Fixed issue #3 (No support for negative numbers).
* Worked around issue #1 (Add 'base' unit) by adding a mode that
  disables scaling; it should have the same effect without introducing
  an artificial unit.

Version 0.1.0.2

* Trivial release for compatibility with QuickCheck 2.7 and older
  HUnit packages as found in Wheezy.
* The release switches the test suite to use Cabal macros, which might
  create issues in some cases.
* Fixed issue #2 (Wrong formatting for small numbers in SI mode).

Version 0.1.0.1

* Trivial release updating upper package bounds (for testing),
  updating homepage/related settings as part of move to github, and
  fixing a few documentation issues.

Version 0.1.0

* Initial release.
