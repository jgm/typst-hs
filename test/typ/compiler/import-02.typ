// Test importing from function scopes.
// Ref: true

#import enum: item
#import assert.with(true): *

#enum(
   item(1)[First],
   item(5)[Fifth]
)
#eq(10, 10)
#ne(5, 6)

