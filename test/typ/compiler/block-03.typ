// Block directly in markup also creates a scope.
#{ let x = 1 }

// Error: 7-8 unknown variable: x
#test(x, 1)

