// Ref: false
// Destructuring with an empty sink.
#let (a: _, ..b) = (a: 1)
#test(b, (:))

