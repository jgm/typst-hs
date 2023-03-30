// Test that fr columns use the correct base.
#grid(
  columns: (1fr,) * 4,
  rows: (1cm,),
  rect(width: 50%, fill: green),
  rect(width: 50%, fill: red),
  rect(width: 50%, fill: green),
  rect(width: 50%, fill: red),
)

