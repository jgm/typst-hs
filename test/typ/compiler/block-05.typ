// Double block creates a scope.
#{{
  import "module.typ": b
  test(b, 1)
}}

// Error: 2-3 unknown variable: b
#b

