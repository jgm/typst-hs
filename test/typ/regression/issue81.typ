#let bug() = {
  if true {
    return heading([1], level: 2)
  }
  return strong([3])
}
#bug()
