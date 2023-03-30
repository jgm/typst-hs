// Test gray color modification.
// Ref: false
#test(luma(20%).lighten(50%), luma(60%))
#test(luma(80%).darken(20%), luma(63.9%))
#test(luma(80%).negate(), luma(20%))
