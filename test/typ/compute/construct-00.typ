// Compare both ways.
#test(rgb(0%, 30%, 70%), rgb("004db3"))

// Alpha channel.
#test(rgb(255, 0, 0, 50%), rgb("ff000080"))

// Test color modification methods.
#test(rgb(25, 35, 45).lighten(10%), rgb(48, 57, 66))
#test(rgb(40, 30, 20).darken(10%), rgb(36, 27, 18))
#test(rgb("#133337").negate(), rgb(236, 204, 200))
#test(white.lighten(100%), white)

