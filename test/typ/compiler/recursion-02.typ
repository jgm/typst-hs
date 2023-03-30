// Test capturing with named function.
#let f = 10
#let f() = f
#test(type(f()), "function")

