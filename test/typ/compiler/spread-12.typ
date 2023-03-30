// Spread at beginning.
#{
  let f(..a, b) = (a, b)
  test(repr(f(1)), "((), 1)")
  test(repr(f(1, 2, 3)), "((1, 2), 3)")
  test(repr(f(1, 2, 3, 4, 5)), "((1, 2, 3, 4), 5)")
}

