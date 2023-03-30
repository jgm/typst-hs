// Multiple unseparated expressions in one line.

// Error: 2-4 invalid number suffix: u
#1u

// Should output `1`.
// Error: 4 expected semicolon or line break
#{1 2}

// Should output `2`.
// Error: 13 expected semicolon or line break
// Error: 23 expected semicolon or line break
#{let x = -1 let y = 3 x + y}

// Should output `3`.
#{
  // Error: 6 expected identifier
  // Error: 10 expected block
  for "v"

  // Error: 8 expected keyword `in`
  // Error: 22 expected block
  for v let z = 1 + 2

  z
}

