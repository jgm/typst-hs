#{
  let x = 2
  for _ in range(61) {
    (x) *= 2
  }
  // Error: 3-17 cannot repeat this string 4611686018427387904 times
  x * "abcdefgh"
}

