// Too many arguments.
#{
  let f(x) = x + 1

  // Error: 8-13 unexpected argument
  f(1, "two", () => x)
}

