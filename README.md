# Typst-hs

Typst-hs is a Haskell library for parsing, evaluating, and converting
typst syntax.  Typst (<https://typst.app>) is a document formatting
and layout language, like TeX.

Currently this library targets v0.4.0 of typst, and offers only
partial support.  There are three main components:

- a parser, which produces an AST from a typst document
- an evaluator, which evaluates the typst expressions in the AST
- a pandoc converter, which converts evaluated typst content to
  a pandoc AST
  
