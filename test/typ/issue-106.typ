// Test 'in' operator on a module (issue #106).
#test("divider" in std, true)
#test("nonexistent" in std, false)
#test("divider" not in std, false)
#let divider = if "divider" in std { divider } else { none }
#test(str(type(divider)), "function")
