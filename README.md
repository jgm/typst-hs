# Typst-hs

Typst-hs is a Haskell library for parsing and evaluating typst
syntax.  Typst (<https://typst.app>) is a document formatting
and layout language, like TeX.

Currently this library targets v0.12.0 of typst, and offers only
partial support.  Typst 0.13.0 symbols are also supported.
There are two main components:

- a parser, which produces an AST from a typst document
- an evaluator, which evaluates the typst expressions in the AST

