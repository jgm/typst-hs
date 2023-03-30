// Let + closure bindings.
#{
  let g = "hi"
  let f() = {
    let g() = "bye"
    g()
  }
  test(f(), "bye")
}

