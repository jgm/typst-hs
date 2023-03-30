// Error: 15-21 maximum function call depth exceeded
#let rec(n) = rec(n) + 1
#rec(1)

