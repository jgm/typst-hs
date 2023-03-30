// Can't import from closures.
#let f(x) = x
// Error: 9-10 cannot import from user-defined functions
#import f: x

