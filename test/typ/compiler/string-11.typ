// Test the `starts-with` and `ends-with` methods.
#test("Typst".starts-with("Ty"), true)
#test("Typst".starts-with(regex("[Tt]ys")), false)
#test("Typst".starts-with("st"), false)
#test("Typst".ends-with("st"), true)
#test("Typst".ends-with(regex("\d*")), true)
#test("Typst".ends-with(regex("\d+")), false)
#test("Typ12".ends-with(regex("\d+")), true)

