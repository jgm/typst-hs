// Test capturing with unnamed function.
#let f = 10
#let f = () => f
#test(type(f()), "integer")

