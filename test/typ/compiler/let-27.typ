// Error: 5 expected identifier
#let

// Error: 6 expected identifier
#{let}

// Error: 5 expected identifier
// Error: 5 expected semicolon or line break
#let "v"

// Error: 7 expected semicolon or line break
#let v 1

// Error: 9 expected expression
#let v =

// Error: 5 expected identifier
// Error: 5 expected semicolon or line break
#let "v" = 1

// Terminated because expression ends.
// Error: 12 expected semicolon or line break
#let v4 = 4 Four

// Terminated by semicolon even though we are in a paren group.
// Error: 18 expected expression
// Error: 18 expected closing paren
#let v5 = (1, 2 + ; Five

// Error: 9-13 expected identifier, found boolean
#let (..true) = false

