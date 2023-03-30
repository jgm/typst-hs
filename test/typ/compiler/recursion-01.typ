// Test with unnamed function.
// Error: 17-18 unknown variable: f
#let f = (n) => f(n - 1)
#f(10)

