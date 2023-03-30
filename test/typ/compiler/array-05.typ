// Test lvalue out of bounds.
#{
  let array = (1, 2, 3)
  // Error: 3-14 array index out of bounds (index: 3, len: 3) and no default value was specified
  array.at(3) = 5
}

