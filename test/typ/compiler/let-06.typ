// Ref: false
// Destructuring with a sink.
#let (a, b, ..c) = (1, 2, 3, 4, 5, 6)
#test(a, 1)
#test(b, 2)
#test(c, (3, 4, 5, 6))

