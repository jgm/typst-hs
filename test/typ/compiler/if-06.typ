// Error: 4 expected expression
#if

// Error: 5 expected expression
#{if}

// Error: 6 expected block
#if x

// Error: 2-6 unexpected keyword `else`
#else {}

// Should output `x`.
// Error: 4 expected expression
#if
x {}

// Should output `something`.
// Error: 6 expected block
#if x something

// Should output `A thing.`
// Error: 19 expected block
A#if false {} else thing

#if a []else [b]
#if a [] else [b]
#if a {} else [b]
