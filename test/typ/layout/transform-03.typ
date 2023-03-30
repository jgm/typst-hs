// Test setting scaling origin.
#let r = rect(width: 100pt, height: 10pt, fill: red)
#set page(height: 65pt)
#box(scale(r, x: 50%, y: 200%, origin: left + top))
#box(scale(r, x: 50%, origin: center))
#box(scale(r, x: 50%, y: 200%, origin: right + bottom))
