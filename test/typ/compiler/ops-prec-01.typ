// Assignment binds stronger than boolean operations.
// Error: 2:3-2:8 cannot mutate a temporary value
#let x = false
#(not x = "a")

