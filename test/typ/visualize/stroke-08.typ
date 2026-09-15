// Test the stroke type: constructor, field access, arithmetic, equality

#test(stroke(2pt + red).paint, red)
#test(stroke(2pt + red).thickness, 2pt)
#test(stroke(paint: blue).thickness, auto)
#test(stroke(3pt).paint, auto)
#test(stroke(red).paint, red)
#test(stroke(3pt).thickness, 3pt)
#test(stroke(paint: auto).paint, auto)
#test(stroke(thickness: auto).thickness, auto)
#test(stroke((paint: auto)).paint, auto)
#test(stroke((thickness: 2pt)).thickness, 2pt)
#test(stroke((paint: blue, thickness: 2pt)).paint, blue)
#test(stroke(stroke(paint: red)).paint, red)

#test((1pt + red).paint, red)
#test((1pt + red).thickness, 1pt)
#test((red + 1pt).paint, red)
#test((red + 1pt).thickness, 1pt)

#test(stroke(1pt) == stroke(1pt), true)
#test(stroke(1pt) == stroke(2pt), false)
#test(stroke(1pt) == stroke(red), false)
// an explicitly set field differs from auto, as in typst
#test(stroke() == stroke(paint: black), false)

#test(type(stroke(1pt)), "stroke")
#test(type(stroke(1pt)), stroke)
#test(type(1pt + red), stroke)

// repr, matching typst's simple stroke forms
#test(repr(2pt + red), "2.0pt + rgb(100%,25%,21%,100%)")
#test(repr(stroke(red)), "rgb(100%,25%,21%,100%)")
#test(repr(stroke(2pt)), "2.0pt")
#test(repr(stroke()), "1pt + black")
