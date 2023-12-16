# Revision history for typst-hs

## 0.5

  * Support "as" keyword in imports (#21).
    [API change] In Typst.Syntax, the Imports type now contains
    fields for an optional "as" identifier.

  * Support version type (#21).
    [API change] Add VVersion constructor to Val, TVersion to ValType.
    Support the `version` constructor function and the `at` method (#21).

  * Parser: Ensure that `set` rule doesn't pick up `if` on next line (#23).

  * Parser: Allow multiline strings (#20).

  * Allow function applications in dictionary key construction (#19).
    [API change]:  in Typst.Syntax, the Dict constructor for Expr
    now takes type `[Spreadable (Expr, Expr)]` instead of
    `[Spreadable (Identifier, Expr)]`. This is because the key
    identifiers sometimes are not known at parse time and must
    be computed in Evaluate.

## 0.4

  * `evaluateTypst` now returns a single Content instead of a sequence
     (breaking API change). The Content is a "document" element that wraps
     the other contents. (This is added automatically in typst:
     https://typst.app/docs/reference/model/document/#parameters-title.)

  * Improve math parser.

  * Add `sys` module and `sys.version`.

  * Math: add `sech` and `csch` operators, `math.mid`.

  * `math.op` is no longer limited to string argument.

  * Remove automatic matching for `|..|` in math (typst 0.8 breaking change).

  * Fix `in` so it works with a type.

  * `repr` label with angle brackets.

  * `cite` now just takes one positional element, a label
    instead of strings (typst 0.9 breaing change).

  * Add `quote` element.

  * Add first-class types. `type()` function now returns a
    ValType instead of a string. Allow ValTypes to == strings
    for compatibility, as in typst.

  * `highlight` element for text.

  * Allow array `zip` method to take any number of arguments.

  * Add `calc.tau`.

  * Add array `intersperse` method.

  * Add string `rev` method.

  * Fix search path for typst packages, from
    `cache/typst/packages/preview/packagename-major.minor.patch` to
    `cache/typst/packages/preview/packagename-major/minor.patch` (#18).

  * Add support of 'wide' spacing character.

  * Fix precedence for numerical attachments (#17).
    Typst makes a subtle distinction between `$a_1(x)$`, in which
    the `_` groups more tightly than the `(..)`, and $`a_f(x)$`,
    in which the `(..)` groups more tightly than the `_`.
    This patch implements the distinction.  This fixes conversion of,
    e.g., `$n(a)^(b)$`.

  * Use typst-symbols 0.1.5.

## 0.3.2.1

  * Fix resolution of symbols (#15). Symbols like `dot`, which only have
    variants, were not being properly resolved: we were getting the
    last matching variant rather than the first.

  * Avoid text's `readFile` in cli app (#13). Instead read as bytestring and
    force interpretation as UTF-8.

  * Fix some parser edge cases involving emphasis after `'` (#12).

## 0.3.2.0

  * Add metadata element.

  * Add dedup method for vector.

  * Add math.class

  * Make MAttach on symbols include limits if symbol is relation.
    This is a 0.7 change: "Changed relations to show attachments as limits
    by default (e.g. in $a ->^x b$)."

  * Add Typst.MathClass.

  * Add im, id, tr text operators.

  * Parse math symbol shorthands as identifiers.

  * Use typst-symbols 0.1.4 so we get all of the defined shorthands.

  * Fix tests because of breaking symbol change ident -> equiv.

  * Depend on dev texmath.

## 0.3.1.0

  * Allow multiplying a ratio by a length.

  * Use `symModule` and `mathModule` directly when evaluating
    Equation instead of looking up `sym` and `math`.

  * Fix parsing of escapes in string literals. Symbols in general
    can't be escaped. There is just a small list of valid escapes.

  * Fix bugs in converting typst regexes to TDFA's format.

  * Allow Symbol to be regex replacement text.

  * Allow VString and VSymbol to be +'d.

  * Update for toml-parser-1.2.0.0 API changes (#9, Eric Mertens).

  * Derive the decoder for typst.toml (#7, Eric Mertens)

  * Implement typst's `toml()` function (#8, Eric Mertens).


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
