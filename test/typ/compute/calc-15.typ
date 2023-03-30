// Test the `min` and `max` functions.
#test(calc.min(2, -4), -4)
#test(calc.min(3.5, 1e2, -0.1, 3), -0.1)
#test(calc.max(-3, 11), 11)
#test(calc.min("hi"), "hi")

