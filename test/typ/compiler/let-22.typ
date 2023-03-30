// Trailing placeholders.
// Error: 10-11 not enough elements to destructure
#let (a, _, _, _, _) = (1,)
#test(a, 1)

