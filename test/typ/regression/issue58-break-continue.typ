// Test that `break` and `continue` only affect the innermost loop
#let f() = {
  for i in range(2) {
    for j in range(2) {
      ((i, j), )
      break
      (10 * j,)
    }
  }
  (-1,)
}

#test(f(), ((0, 0), (1, 0), -1))

#let g() = {
  for i in range(2) {
    for j in range(2) {
      ((i, j), )
      continue
      (10 + j,)
    }
    (100 + i,)
  }
  (-1,)
}

#test(g(), ((0, 0), (0, 1), 100, (1, 0), (1, 1), 101, -1))

#let f() = {
  let i = 0
  while i < 2 {
    i += 1
    let j = 0
    while j < 2 {
      ((i, j), )
      j += 1
      break
      (10 * j,)
    }
    (100 + i,)
  }
  (-1,)
}

#test(f(), ((1, 0), 101, (2, 0), 102, -1))

#let g() = {
  let i = 0
  while i < 2 {
    i += 1
    let j = 0
    while j < 2 {
      ((i, j), )
      j += 1
      continue
      (10 + j,)
    }
    (100 + i,)
  }
  (-1,)
}

#test(g(), ((1, 0), (1, 1), 101, (2, 0), (2, 1), 102, -1))
