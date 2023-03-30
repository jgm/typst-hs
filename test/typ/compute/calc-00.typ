// Test conversion to numbers.
#test(int(false), 0)
#test(int(true), 1)
#test(int(10), 10)
#test(int("150"), 150)
#test(int(10 / 3), 3)
#test(float(10), 10.0)
#test(float(50% * 30%), 0.15)
#test(float("31.4e-1"), 3.14)
#test(type(float(10)), "float")

