# Revision history for typst-hs

## 0.3.0.0

  * We now target typst 0.6.

  * `joinVals` - fall back on repr when as a fallback in joining values.

  * Fix a spacing issue in parsing code inside equations (#6).

  * Fix `#include`. It wasn't including content!

  * Fix issue with math parsing of factorial (#5).

  * Handle "style" by evaluating it immediately, rather than passing it through
    as an element in content (#4).

  * Add `outline.entry`.

  * Allow identifiers to start with `_`.

  * Fix bug in parsing consecutive '#' expressions in math function (#2).

  * Fix bugs in makeLiteralRE.

  * Give namedArg an argument for a default value.
    This avoids spurious parse error messages.

  * Change return value of dictionary insert method to none.

  * Improve #panic output.

  * [API change]:  Add Spreadable type in Typst.Syntax.
    Use this for Dict and Array values.

  * Handle package lookup, assuming packages are either local or cached.

  * API change: combine IO operations into Operations structure.
    `evaluateTypst` now takes a single Operations dictionary instead
    of separate `loadBytes` and `currentUTCTime` functions. And
    Operations now also includes functions to query environment
    variables and check directories.  This will be needed for
    package lookup.

  * Depend on typst-symbols 0.1.2.

  * Make factorial take priority over fraction.

## 0.2.0.0

  * We now target typst 0.5.

  * Implement methods for datetime.

  * Implement `base` parameter on str.

  * Add `datetime` constructor.

  * Implement `datetime.today`.

  * Add VDateTime type.

  * Implement `fields` method on content.

  * Add `display`, `inline`, `script`, `sscript` to math module.

  * Add `str.to-unicode`, `str.from-unicode`.

  * Add `calc.ln` and `calc.exp`.

  * Remove deprecated `calc.mod`.

  * Depend on typst-symbols 0.1.1.


## 0.1.0.0

* First version. Released on an unsuspecting world.
