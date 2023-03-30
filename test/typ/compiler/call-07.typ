// Error: 7 expected expression
// Error: 8 expected expression
#func(:)

// Error: 10-12 unexpected end of block comment
#func(a:1*/)

// Error: 8 expected comma
#func(1 2)

// Error: 7-8 expected identifier, found integer
// Error: 9 expected expression
#func(1:)

// Error: 7-8 expected identifier, found integer
#func(1:2)

// Error: 7-12 expected identifier, found string
#func("abc": 2)

// Error: 7-10 expected identifier, found group
#func((x):1)

