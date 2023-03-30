// Trailing comma.
#test(1 + 1, 2,)

// Call function assigned to variable.
#let alias = type
#test(alias(alias), "function")

// Callee expressions.
#{
  // Wrapped in parens.
  test((type)("hi"), "string")

  // Call the return value of a function.
  let adder(dx) = x => x + dx
  test(adder(2)(5), 7)
}

