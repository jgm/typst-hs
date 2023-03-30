// Ref: false
// Destructuring with unnamed sink.
#let (a, ..) = (a: 1, b: 2)
#test(a, 1)

