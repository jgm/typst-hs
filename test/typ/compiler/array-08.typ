// Test bad lvalue.
// Error: 2:3-2:15 type array has no method `yolo`
#let array = (1, 2, 3)
#(array.yolo() = 4)

