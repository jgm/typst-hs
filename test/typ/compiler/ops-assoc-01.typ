// Assignment is right-associative.
{
  let x = 1
  let y = 2
  x = y = "ok"
  test(x, none)
  test(y, "ok")
}
