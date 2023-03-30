#{
  let f(..a, b, c, d) = none

  // Error: 4-10 missing argument: d
  f(1, 2)
}
