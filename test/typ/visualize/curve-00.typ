#set page(height: 200pt, width: 200pt)
#table(
  columns: (1fr, 1fr),
  rows: (1fr, 1fr),
  align: center + horizon,
  curve(
    fill: red,
    curve.move((0%, 0%)),
    curve.cubic((4%, -4%), (46%, 46%), (50%, 50%)),
    curve.cubic(auto, (4%, 54%), (0%, 50%)),
    curve.cubic(auto, (46%, -4%), (50%, 0%)),
    curve.close(),
  ),
  curve(
    fill: purple,
    stroke: 1pt,
    curve.move((0pt, 0pt)),
    curve.line((30pt, 30pt)),
    curve.line((0pt, 30pt)),
    curve.line((30pt, 0pt)),
  ),
  curve(
    fill: blue,
    stroke: 1pt,
    curve.move((30%, 0%)),
    curve.cubic((65%, 30%), (10%, 60%), (30%, 60%)),
    curve.cubic(auto, (110%, 0%), (50%, 30%)),
    curve.close(),
  ),
  curve(
    stroke: 5pt,
    curve.move((0pt, 30pt)),
    curve.line((30pt, 30pt)),
    curve.line((15pt, 0pt)),
    curve.close(),
  ),
)
