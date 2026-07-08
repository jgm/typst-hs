// Test the curve components.
#curve(
  stroke: 2pt + red,
  curve.move((0pt, 0pt)),
  curve.quad((20pt, 40pt), (40pt, 0pt), relative: true),
  curve.quad(auto, (40pt, 0pt), relative: true),
  curve.cubic(none, (90pt, 0pt), (50pt, 0pt)),
  curve.close(mode: "straight"),
)
