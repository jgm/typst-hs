// Test default value.
#test((1, 2, 3).at(2, default: 5), 3)
#test((1, 2, 3).at(3, default: 5), 5)

