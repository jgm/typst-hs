// Block in expression does create a scope.
#let a = {
  let b = 1
  b
}

#test(a, 1)

// Error: 3-4 unknown variable: b
#{b}

