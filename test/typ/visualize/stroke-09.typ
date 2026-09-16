// Test the cap, join, and miter-limit stroke fields.

// constructor and dictionary fields
#test(stroke(cap: "round").cap, "round")
#test(stroke(join: "bevel").join, "bevel")
#test(stroke(miter-limit: 2.5).miter-limit, 2.5)
#test(stroke(miter-limit: 4).miter-limit, 4.0)
#test(type(stroke(cap: "round").cap), "string")
#test(type(stroke(miter-limit: 2.5).miter-limit), "float")
#test(stroke((cap: "square", miter-limit: 2.0)).cap, "square")
#test(stroke((join: "round")).join, "round")

// unset fields are auto
#test(stroke(1pt + red).cap, auto)
#test(stroke(1pt + red).join, auto)
#test(stroke(1pt + red).miter-limit, auto)

// a named argument overrides the base, and auto resets it
#test(stroke(stroke(cap: "round")).cap, "round")
#test(stroke(stroke(cap: "round"), cap: auto).cap, auto)
#test(stroke((cap: "round"), join: "bevel").join, "bevel")
#test(stroke((cap: auto)).cap, auto)

// equality, including an explicitly set field vs auto
#test(stroke(cap: "round") == stroke(cap: "round"), true)
#test(stroke(cap: "round") == stroke(), false)
#test(stroke(join: "bevel") == stroke(join: "round"), false)
#test(stroke(miter-limit: 4) == stroke(miter-limit: 4.0), true)
#test(stroke(miter-limit: 4) == stroke(), false)
#test(stroke(cap: "round", join: "bevel") == stroke(join: "bevel", cap: "round"), true)

// repr, matching typst's parenthesized stroke form
#test(repr(stroke(cap: "round")), "(cap: \"round\")")
#test(repr(stroke(join: "bevel")), "(join: \"bevel\")")
#test(repr(stroke(miter-limit: 3.0)), "(miter-limit: 3.0)")
#test(repr(stroke(miter-limit: 4)), "(miter-limit: 4.0)")
#test(repr(stroke(paint: red, cap: "round")), "(paint: rgb(100%,25%,21%,100%), cap: \"round\")")
#test(repr(stroke(thickness: 2pt, miter-limit: 4.0)), "(thickness: 2.0pt, miter-limit: 4.0)")
#test(repr(stroke((paint: red, cap: "round", miter-limit: 2.0, thickness: 1pt))), "(paint: rgb(100%,25%,21%,100%), thickness: 1.0pt, cap: \"round\", miter-limit: 2.0)")
// the simple forms still apply when these fields are unset
#test(repr(stroke(1pt + red)), "1.0pt + rgb(100%,25%,21%,100%)")

// typst-hs extension: adding to a stroke refines or merges fields
#test(repr((2pt + red) + blue), "2.0pt + rgb(0%,45%,85%,100%)")
#test(repr((2pt + red) + 3pt), "3.0pt + rgb(100%,25%,21%,100%)")
#test(repr(stroke(cap: "round") + 2pt), "(thickness: 2.0pt, cap: \"round\")")
#test(repr((1pt + red) + stroke(cap: "round")), "(paint: rgb(100%,25%,21%,100%), thickness: 1.0pt, cap: \"round\")")
#test(repr(stroke(cap: "round") + stroke(join: "bevel")), "(cap: \"round\", join: \"bevel\")")
#test(repr(stroke(cap: "round") + stroke(cap: "square")), "(cap: \"square\")")
#test(repr(stroke(paint: red, cap: "round") + stroke(paint: blue)), "(paint: rgb(0%,45%,85%,100%), cap: \"round\")")
#test((2pt + red) + 3pt == 3pt + red, true)
