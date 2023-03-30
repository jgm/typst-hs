#let _ = 4

#for _ in range(2) []

// Error: 2-3 unexpected underscore
#_

// Error: 8-9 unexpected underscore
#lorem(_)

// Error: 3-4 expected expression, found underscore
#(_,)

// Error: 3-4 expected expression, found underscore
#{_}

// Error: 8-9 expected expression, found underscore
#{ 1 + _ }

