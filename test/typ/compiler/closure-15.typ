// Error: 14-15 duplicate parameter: a
// Error: 23-24 duplicate parameter: b
// Error: 35-36 duplicate parameter: b
#let f(a, b, a: none, b: none, c, b) = none

