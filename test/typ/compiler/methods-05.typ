// Error: 2:3-2:19 cannot mutate a temporary value
#let numbers = (1, 2, 3)
#(numbers.sorted() = 1)

