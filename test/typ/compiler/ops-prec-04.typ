// Parentheses override precedence.
#test((1), 1)
#test((1+2)*-3, -9)

// Error: 14 expected closing paren
#test({(1 + 1}, 2)
