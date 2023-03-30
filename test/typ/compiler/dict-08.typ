// Simple expression after already being identified as a dictionary.
// Error: 9-10 expected named or keyed pair, found identifier
#(a: 1, b)

// Identified as dictionary due to initial colon.
// Error: 4-5 expected named or keyed pair, found integer
// Error: 5 expected comma
// Error: 12-16 expected identifier or string, found boolean
// Error: 17 expected expression
#(:1 b:"", true:)

// Error: 3-8 expected identifier or string, found binary expression
#(a + b: "hey")

