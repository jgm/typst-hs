# Revision history for typst-hs

## 0.6.2

  * Allow types to act as constructor functions, as in typst (#61).
    Add new unexported module, Typst.Constructors, defining the
    constructors for each of the typst types.
    Fix name of string type: it is `str`, not `string` (which is only
    the `repr`).

  * Support `dict`, `datetime`, `symbol` constructors.

  * Improve path handling when loading files (#60).
    We now look in the "local path" (the path of the containing file)
    except when the path begins with `/` (in which case it is resolved
    relative to the package path).

  * Fix issue with expression parsing involving labels (#59).

  * Remove spurious trace in `getPath`.

## 0.6.1

  * Fix precedence for functions (#55).
    `1(x)` and `!(x)` should not be parsed as functions.
    Note that we still don't match typst's behavior for `f_"!"(x)`.
    For us this works just like `f_!(x)`, but for typst we get
    a function in the subscript for the former but not the latter.
    Fixing this would require some changes in the types.

  * Define sys.version and sys.inputs.typst-hs-version (#56).
    The former is set to the version of typst we are trying to
    implement. The latter is a stringified version number from typst-hs.
    This will allow typst programs to tell when they're running
    on typst-hs (or pandoc), and react accordingly.

  * Rename stBeforeSpace -> stSpaceBefore to avoid confusion.

  * Fix precedence issues in math parsing (#54).
    Increased precedence of ! (factorial).
    `_` or `^` should eagerly gobble a grouped argument (`c_(a)`).

  * Minimal support for `context` (#53). Parse `context` keyword.
    New Context constructor in Expr [API change].
    Evaluate this by just evaluating the expression, for now.
    Note that we don't support the features (like location or
    numbering) that context is used to affect anyway, so this change
    probably won't be enough for meaningful support. But it might
    prevent some documents from issuing errors.

  * Arguments at method.

  * Array windows, reduce, to-dict methods.

  * Allow parentheses in import.

  * Make standard module available under std (typst 0.12).

  * Add over/underparen, over/undershell in math module.

  * Add stretch function.

  * Add skew.

  * Depend on typst-symbols 0.1.7 and start to target typst 0.12.

  * Reset indentation requirements inside `[]` content block. e.g.
    ```
    / B: #block[
    - a
    ]
    ```
    We don't need indentation inside the block content.


## 0.6

  * Recognize figure.caption function (#52).

  * Allow defined identifiers to override math defaults (#51).
    Previously we evaluated all math in a special environment that
    shadowed any user-defined functions with the same-named functions
    from the math or sym modules. This change gives user-defined identifiers
    priority over the math defaults, allowing things like `bb` to be
    overridden.

  * Typst.Types: EvalState now has two new fields, `evalMathIdentifiers` and
    `evalStandardIdentifiers`. `evalIdentifiers` is now just for user-defined
    identifiers. [API change]

  * Don't implicitly load sys module for math.

## 0.5.0.5

  * Allow numbers like `1.` in math mode (#50).

## 0.5.0.4

  * Add built-in identifiers for standard types (#21):
    array, bool, content, int, float, regex, length,
    alignment, color, symbol, string.

  * Adjust emphasis parser for CJK characters (#49).
    Typst documentation says that `*` strong emphasis
    "only works at word boundaries." However, in typst/typst#2648
    this was changed for CJK.

## 0.5.0.3

  * Support grid.(cell,header,footer,hline,vline) (#44).

  * Support table.(cell,vline,hline,header,footer) (#44).

  * Allow space after equation in code (#43).

  * Treat unicode whitespace characters as whitespace (#43).

  * Allow raw (backticked) content as code expression (#43).

  * Allow colon in label (#43).

  * Allow line comments at end of file (#43).

  * Depend on typst-symbols 0.1.6.

  * Add Haddock docs to parts of the public API (Eli Adelhult,
    Leopold Wigratt).

  * Avoid backtracking in `pDictExpr` (Jonathan Widén).

  * Allow colon in dict literal (Jonathan Widén) (#43, #45).

## 0.5.0.2

  * Fix parsing of field access in math (#41). `$plus.circle_2$`
    should give you a subscript 2 on the symbol `plus.circle`.
    Underscores are not allowed in field access in math.

  * Support toml-parser-2.0.0.0 (Eric Mertens).

## 0.5.0.1

  * Set `evalPackageRoot` to working dir to start, even if the file to be
    converted is somewhere else. This seems to be what the test suite expects.

  * Make file loading relative to package root if present (#39).

  * Parser: remove `pBindExpr` from `pBaseExpr`. It does not seem
    to be necessary, and it causes problems with things like `$#x = $y$` (#34).

  * Fix assignment of module name in package imports (#30).

  * Don't allow `container.at` to insert new values (#26).

  * Handle `dict.at(variable) = expression` (#25).

  * Remove dependency on the unmaintained digits library (#24).
    We just copy the code for the function we need (with
    attribution): it is BSD3-licensed.

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
