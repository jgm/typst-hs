// Ref: false
// Destructuring with an empty sink.
#let (a, ..b, c) = (1, 2)
#test(a, 1)
#test(b, ())
#test(c, 2)

