// Test box sizing with layoutable child.
#box(
  width: 50pt,
  height: 50pt,
  fill: yellow,
  curve(
    fill: purple,
    curve.move((0pt, 0pt)),
    curve.line((30pt, 30pt)),
    curve.line((0pt, 30pt)),
    curve.line((30pt, 0pt)),
  ),
)
