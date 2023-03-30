// Closures cannot have fields.
#let f(x) = x
// Error: 4-11 cannot access fields on user-defined functions
#f.invalid

