// Parameter unpacking.
#let f((a, b), ..c) = (a, b, c)
#test(f((1, 2), 3, 4), (1, 2, (3, 4)))

#let f((k: a, b), c: 3, (d,)) = (a, b, c, d)
#test(f((k: 1, b: 2), (4,)), (1, 2, 3, 4))

// Error: 22-23 duplicate parameter: a
#let f((a: b), (c,), a) = none

// Error: 8-14 expected identifier, found array
#let f((a, b): 0) = none

// Error: 10-19 expected identifier, found destructuring pattern
#let f(..(a, b: c)) = none

// Error: 10-16 expected identifier, found array
#let f(..(a, b)) = none

// Error: 10-19 expected identifier, found destructuring pattern
#let f(..(a, b: c)) = none

