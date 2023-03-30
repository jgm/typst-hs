// Can't import from closures, despite modifiers.
#let f(x) = x
// Error: 9-18 cannot import from user-defined functions
#import f.with(5): x

