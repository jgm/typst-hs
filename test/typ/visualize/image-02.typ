// Test all three fit modes.
#set page(height: 50pt, margin: 0pt)
#grid(
  columns: (1fr, 1fr, 1fr),
  rows: 100%,
  gutter: 3pt,
  image("test/assets/files/tiger.jpg", width: 100%, height: 100%, fit: "contain"),
  image("test/assets/files/tiger.jpg", width: 100%, height: 100%, fit: "cover"),
  image("test/assets/files/monkey.svg", width: 100%, height: 100%, fit: "stretch"),
)

