// Value of while loops.
// Ref: false

#test(while false {}, none)

#let i = 0
#test(type(while i < 1 [#(i += 1)]), "content")

