// Test `with` method.

// Apply positional arguments.
#let add(x, y) = x + y
#test(add.with(2)(3), 5)
#test(add.with(2).with(3)(), 5)
#test((add.with(2))(4), 6)
#test((add.with(2).with(3))(), 5)

// Make sure that named arguments are overridable.
#let inc(x, y: 1) = x + y
#test(inc(1), 2)

#let inc2 = inc.with(y: 2)
#test(inc2(2), 4)
#test(inc2(2, y: 4), 6)
