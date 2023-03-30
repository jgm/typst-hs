// Parameter bindings.
#{
  let x = 5
  let g() = {
    let f(x, y: x) = x + y
    f
  }

  test(g()(8), 13)
}

