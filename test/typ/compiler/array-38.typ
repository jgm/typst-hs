// Error: 4 expected closing paren
#{(}

// Error: 3-4 unexpected closing paren
#{)}

// Error: 4-6 unexpected end of block comment
#(1*/2)

// Error: 6-8 invalid number suffix: u
#(1, 1u 2)

// Error: 3-4 unexpected comma
#(,1)

// Missing expression makes named pair incomplete, making this an empty array.
// Error: 5 expected expression
#(a:)

// Named pair after this is already identified as an array.
// Error: 6-10 expected expression, found named pair
#(1, b: 2)

// Keyed pair after this is already identified as an array.
// Error: 6-14 expected expression, found keyed pair
#(1, "key": 2)
