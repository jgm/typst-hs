// Import bindings.
#{
  let b = "module.typ"
  let f() = {
    import b: b
    b
  }
  test(f(), 1)
}

