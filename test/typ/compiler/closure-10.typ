// Too few arguments.
#{
  let types(x, y) = "[" + type(x) + ", " + type(y) + "]"
  test(types(14%, 12pt), "[ratio, length]")

  // Error: 13-21 missing argument: y
  test(types("nope"), "[string, none]")
}

