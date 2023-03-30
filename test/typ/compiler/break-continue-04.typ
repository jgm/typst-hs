// Test break outside of loop.
#let f() = {
  // Error: 3-8 cannot break outside of loop
  break
}

#for i in range(1) {
  f()
}

