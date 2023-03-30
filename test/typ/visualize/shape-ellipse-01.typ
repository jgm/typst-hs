#set rect(inset: 0pt)
#set ellipse(inset: 0pt)

Rect in ellipse in fixed rect.
#rect(width: 3cm, height: 2cm, fill: rgb("2a631a"),
  ellipse(fill: red, width: 100%, height: 100%,
    rect(fill: green, width: 100%, height: 100%,
      align(center + horizon)[
        Stuff inside an ellipse!
      ]
    )
  )
)

Auto-sized ellipse.
#ellipse(fill: green, stroke: 3pt + red, inset: 3pt)[
  #set text(8pt)
  But, soft! what light through yonder window breaks?
]


An inline
#box(ellipse(width: 8pt, height: 6pt, outset: (top: 3pt, rest: 5.5pt)))
ellipse.
