// Test that that block spacing and text style are respected from
// the outside, but the more specific fill is respected.
#set block(spacing: 4pt)
#set text(style: "italic", fill: eastern)
#let x = [And the red #parbreak() lay silent!]
#text(fill: red, x)

