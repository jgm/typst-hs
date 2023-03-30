// Test doing things with arguments.
#{
  let save(..args) = {
    test(type(args), "arguments")
    test(repr(args), "(three: true, 1, 2)")
  }

  save(1, 2, three: true)
}

