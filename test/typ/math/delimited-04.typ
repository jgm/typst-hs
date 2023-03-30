// Test that symbols aren't matched automatically.
$ bracket.l a/b bracket.r
  = lr(bracket.l a/b bracket.r) $

