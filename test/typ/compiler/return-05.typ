// Test value return from content.
#let x = 3
#let f() = [
  Hello 😀
  #return "nope"
  World
]

#test(f(), "nope")
