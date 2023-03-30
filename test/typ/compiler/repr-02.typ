// Colors and strokes.
#set text(0.8em)
#rgb("f7a205") \
#(2pt + rgb("f7a205"))

// Strings and escaping.
#raw(repr("hi"), lang: "typc")
#repr("a\n[]\"\u{1F680}string")

// Content.
#raw(lang: "typc", repr[*Hey*])

// Functions are invisible.
Nothing
#let f(x) = x
#f
#rect
#(() => none)
