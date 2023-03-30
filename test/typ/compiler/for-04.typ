// Destructuring without parentheses.
// Error: 7 expected keyword `in`. did you mean to use a destructuring pattern?
#for k, v in (a: 4, b: 5) {
  dont-care
}

// Error: 5 expected identifier
#for

// Error: 5 expected identifier
#for//

// Error: 6 expected identifier
#{for}

// Error: 7 expected keyword `in`
#for v

// Error: 10 expected expression
#for v in

// Error: 15 expected block
#for v in iter

// Error: 5 expected identifier
#for
v in iter {}

// Error: 6 expected identifier
// Error: 10 expected block
A#for "v" thing

// Error: 5 expected identifier
#for "v" in iter {}

// Error: 7 expected keyword `in`
#for a + b in iter {}
