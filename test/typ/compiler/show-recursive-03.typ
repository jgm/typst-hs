// Test multi-recursion with nested lists.
// Note to self: the problem here is that the block is applied first,
// and then the others don't match because we're not looking recursively in
// the result of applying the show rule...
#set rect(inset: 3pt)
#show list: rect.with(stroke: blue)
#show list: rect.with(stroke: red)
#show list: block

- List
  - Nested
  - List
- Recursive!
