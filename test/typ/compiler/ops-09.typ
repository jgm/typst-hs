// Test comparison operators.

#test(13 * 3 < 14 * 4, true)
#test(5 < 10, true)
#test(5 > 5, false)
#test(5 <= 5, true)
#test(5 <= 4, false)
#test(45deg < 1rad, true)
#test(10% < 20%, true)
#test(50% < 40% + 0pt, false)
#test(40% + 0pt < 50% + 0pt, true)
#test(1em < 2em, true)

