# Revision history for typst-hs

## 0.9

  * Tweak parsing of list items and headings to match Typst more closely
    (#92, #58, #90, #92, Norbert Pozar).

    - Handles trivia (whitespace, comments) as Typst (see #92).
    - Replaces `stLineStartCol` by `stAtStart`: `stAtStart` is set to `True`
      after an end of line, at the beginning of content or at the beginning
      of a list item (since list items can be nested), and set to `False`
      after any non-trivia markup. This appears simpler and more robust,
      especially after comments.
    - Removes `stIndent` state.
       Indented block is handled completely in `pIndented` parser and does
      not rely on `stIndent` state.

  * Support `func()` for content elements in submodules (math etc.)
    (#57, #95, Norbert Pozar). `func()` method now element names with
    multiple fragements like `math.attach`.

  * Match raw block handling of Typst 0.14 (#94, Norbert Pozar).
    This commit should produce the same result.

    - Automatically dedent raw text based on the shortest indent of the raw
      code. In particular, do not use `stIndent` for this since Typst does not
      dedent the code based on the indentation of the containing list item.
    - Accept any identifier-like language tags.
    - Strip the last line if it is only whitespace.
    - If the last line's nonwhitespace chars ends with `, strip one
      space from the end.

  * Tweak comment parsing to match Typst (#93, Norbert Pozar).

    - Line comments inside block comments are not parsed as line comments
      anymore (partially addresses #90).
    - Block comments do not need to be closed by */ and can be active until
      the end of file (partially addresses #90).
    - Error is properly reported when unmatched */ appears instead of
      misparsing this as a beginning of strong elem.
    - Stop line comments from eating the end of line character
      This is important for correctly issuing parbreaks and spaces that
      follow line comments and a preparation for simpler handling of indented
      blocks.

  * Hide `#let test..` definition from test parse output (Norbert Pozar).
    After this change the test function `#let test(...` is only injected
    at evaluation and its parsed AST no longer appears in `.out` files.
    Other advantages:

    - Source locations match the input file.
    - Future changes to AST will not show up in every output.
    - Easier to read test output.
    - 5% faster test run (5s -> 4.75s)

  * Simpler tracking of bracket nesting in markup (Norbert Pozar, #86).
    Keep the count of unclosed brackets in `PState` instead of using a
    `between` parser to prevent exponential parsing time due to backtracing.

  * Improve handling of control flow statements at loop block boundaries
    (Norbert Pozar).

    - `break` and `continue` now only affect the innermost `for` and `while`
      loop by resetting `evalFlowDirective` at the loop end.
    - `return` now returns even when used inside a loop: it stops the loop the
      same way `break` does but its state is not reset at the end of the
      loop so propagates up to the function boundary.

  * `array.join()`: do not prepend separator for length 1 arrays (Norbert
    Pozar). Separator is now only inserted when the array has at least 2
    elements.

 *  Support nested destructuring binds and expressions in LHS of assignments
    (Norbert Pozar).

  * Parse (..expr) as Array (Norbert Pozar, #47).
    Trailing comma is not required in an array expression with single spread
    operator. Also spreads `none`.

  * Support nested destructuring (Norbert Pozar).
    Typst supports nesting of destructuring binds both in array
    destructuring `(_, (_, _), _)` and dictionary destructuring
    `(x: (_, _), y)`. This commit adds support for such nested
    destructuring of arbitrary depth.

    Changes AST for `BindPart` to allow for recursion [API change].

    -  `Simple (Maybe Identifier)` -> `Simple Bind`
    -  `WithKey Identifier (Maybe Identifier)` -> `WithKey Identifier Bind`

    It also disallows "unnamed patterns" when destructuring from a
    dictionary (to match Typst behavior):

    ```typst
    #let (_, x) = (x: 1, y: 1)
    //    ^ unnamed
    #let ((a, b), x) = (x: 1, y: 1)
    //    ^^^^^^ unnamed
    ```

## 0.8.1

  * Fix parsing and evaluation of closures with _ (#84, Norbert Pozar).

  * Improve parsing of float literals (#83, Norbert Pozar).

  * Fix incorrect parsing of underscores in code (#82, Norbert Pozar).

  * Fix early return from function (#81).

  * Add default parameter for `first`, `last` methods on string, array
    (typst 0.14 addition).

  * Make the top-level document susceptible to set rules (#80).

  * Handle `#title` (#80).

  * Use typst-symbols 0.1.9 (typst 0.14 symbols).

## 0.8.0.2

  * Give correct source positions even with nested includes (#74).

## 0.8.0.1

  * Fix subtracting units, e.g. `1mm-0mm` (#71, Luke Grehan).

## 0.8

  * Allow `json`, `toml`, `xml` to take either file path or bytes.

  * Allow `read` to return bytes if encoding is 'none'.

  * `bibliography`, `image`: change parameter name to `source` and allow bytes.

  * Add 'bytes' as a type name and constructor.

  * Add VBytes constructor for Val and TBytes for ValType. [API change]

  * Allow values of arguments type to be added together.

  * Support `calc.norm`.

  * Math: add `lcm` operator.

  * Require typst-symbols >= 0.1.8.1 (#67), giving us typst 0.13 symbols.

  * Add "dictionary" as name of TDict type (#65).


## 0.7

  * Fix problems with module loading paths (#62).

  * Skip whitespace before parsing key/value values in math (#64).

  * Parse the `delim` attribute (and any others) in `math.mat` (#64).

  * Methods: fix 'has' method so it works for sequences of elements.

  * Evaluate: run show rules after looking up an identifier.

  * Show rule changes:

    + ShowRule now has an extra parameter for a unique identifier.
      This allows us to prevent double application of show rules,
      while allowing distinct rules with the same selector. [API change]
    + In applying show rules, we no longer recurse into an element's
      fields, as this caused double application of show rules in nested
      contexts. (See #63.) However, this is not a complete fix because there
      are some tests that still fail.
    + Ensure that show rules are applied to text elements.

  * Reorganized tests. Now put the `.out` and `.typ` files in same directory,
    rather than having separate trees.

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
