// Edge case for module access that isn't fixed.
#import "module.typ"

// Works because the method name isn't categorized as mutating.
#test((module,).at(0).item(1, 2), 3)

// Doesn't work because of mutating name.
// Error: 2-11 cannot mutate a temporary value
#(module,).at(0).push()

