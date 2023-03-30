// Ref: false
// Destructuring with unnamed sink.
#let (a, .., b) = (1, 2, 3, 4)
#test(a, 1)
#test(b, 4)

// Error: 10-11 at most one binding per identifier is allowed
#let (a, a) = (1, 2)

// Error: 12-15 at most one destructuring sink is allowed
#let (..a, ..a) = (1, 2)

// Error: 12-13 at most one binding per identifier is allowed
#let (a, ..a) = (1, 2)

// Error: 13-14 at most one binding per identifier is allowed
#let (a: a, a) = (a: 1, b: 2)

// Error: 13-20 expected identifier, found function call
#let (a, b: b.at(0)) = (a: 1, b: 2)

// Error: 7-14 expected identifier or destructuring sink, found function call
#let (a.at(0),) = (1,)

