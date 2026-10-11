// arguments.at with a negative index must return the default (or error), not
// crash with a Prelude.!!: negative index exception.
#let f(..a) = a.at(-1, default: "none")
#f(1, 2, 3)
