// Test break in function call.
#let identity(x) = x
#let out = for i in range(5) {
  "A"
  identity({
    "B"
    break
  })
  "C"
}

#test(out, "AB")

