// Don't leak environment.
#{
  // Error: 16-17 unknown variable: x
  let func() = x
  let x = "hi"
  func()
}

