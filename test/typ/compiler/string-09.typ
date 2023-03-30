// Test the `clusters` and `codepoints` methods.
#test("abc".clusters(), ("a", "b", "c"))
#test("abc".clusters(), ("a", "b", "c"))
#test("🏳️‍🌈!".clusters(), ("🏳️‍🌈", "!"))
#test("🏳️‍🌈!".codepoints(), ("🏳", "\u{fe0f}", "\u{200d}", "🌈", "!"))

