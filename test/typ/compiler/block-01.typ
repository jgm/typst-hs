// Nothing evaluates to none.
#test({}, none)

// Let evaluates to none.
#test({ let v = 0 }, none)

// Evaluates to single expression.
#test({ "hello" }, "hello")

// Evaluates to string.
#test({ let x = "m"; x + "y" }, "my")

// Evaluated to int.
#test({
  let x = 1
  let y = 2
  x + y
}, 3)

// String is joined with trailing none, evaluates to string.
#test({
  type("")
  none
}, "string")

