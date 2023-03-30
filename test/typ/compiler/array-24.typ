// Test the `sum` method.
#test(().sum(default: 0), 0)
#test(().sum(default: []), [])
#test((1, 2, 3).sum(), 6)

