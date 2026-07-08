// Test the inclusive parameter of range.
#test(range(3, inclusive: true), (0, 1, 2, 3))
#test(range(3, inclusive: false), (0, 1, 2))
#test(range(2, 5, inclusive: true), (2, 3, 4, 5))
#test(range(5, 2, step: -1, inclusive: true), (5, 4, 3, 2))
#test(range(0, 10, step: 3, inclusive: true), (0, 3, 6, 9))
#test(range(0, 9, step: 3, inclusive: true), (0, 3, 6, 9))
#test(range(0, 9, step: 3, inclusive: false), (0, 3, 6))
