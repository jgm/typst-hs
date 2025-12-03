// Test that `return` works from inside loops
#let h() = {
  for i in range(2) {
    for j in range(2) {
      ((i, j), )
      return
      (10 * j,)
    }
  }
  (-1,)
}

#test(h(), ((0, 0),))

#let h() = {
  let i = 0
  while i < 2 {
    i += 1
    let j = 0
    while j < 2 {
      ((i, j), )
      j += 1
      return
      (10 * j,)
    }
    (100 + i,)
  }
  (-1,)
}

#test(h(), ((1, 0),))

// Test that `return val` works from inside loops
#let h() = {
  for i in range(2) {
    for j in range(2) {
      ((i, j), )
      return [a]
      (10 * j,)
    }
  }
  (-1,)
}

#test(h(), [a])

#let h() = {
  let i = 0
  while i < 2 {
    i += 1
    let j = 0
    while j < 2 {
      ((i, j), )
      j += 1
      return [a]
      (10 * j,)
    }
    (100 + i,)
  }
  (-1,)
}

#test(h(), [a])
