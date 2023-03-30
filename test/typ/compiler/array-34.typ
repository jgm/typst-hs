// Test the `zip` method.
#test(().zip(()), ())
#test((1,).zip(()), ())
#test((1,).zip((2,)), ((1, 2),))
#test((1, 2).zip((3, 4)), ((1, 3), (2, 4)))
#test((1, 2, 3, 4).zip((5, 6)), ((1, 5), (2, 6)))
#test(((1, 2), 3).zip((4, 5)), (((1, 2), 4), (3, 5)))
#test((1, "hi").zip((true, false)), ((1, true), ("hi", false)))


