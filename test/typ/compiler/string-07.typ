// Test the `slice` method.
#test("abc".slice(1, 2), "b")
#test("abc🏡def".slice(2, 7), "c🏡")
#test("abc🏡def".slice(2, -2), "c🏡d")
#test("abc🏡def".slice(-3, -1), "de")

