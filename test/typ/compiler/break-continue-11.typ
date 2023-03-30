// Test second block during break flow.
// Ref: true

#for i in range(10) {
  table(
    { [A]; break },
    for _ in range(3) [B]
  )
}
