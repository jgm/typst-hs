// Ref: false
// Destructuring with a sink.
#let (a: _, ..b) = (a: 1, b: 2, c: 3)
#test(b, (b: 2, c: 3))

