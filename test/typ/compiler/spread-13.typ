// Spread in the middle.
#{
  let f(a, ..b, c) = (a, b, c)
  test(repr(f(1, 2)), "(1, (), 2)")
  test(repr(f(1, 2, 3, 4, 5)), "(1, (2, 3, 4), 5)")
}

