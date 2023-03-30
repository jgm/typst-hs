// Test reading CSV data.
// Ref: true
#set page(width: auto)
#let data = csv("test/assets/files/zoo.csv")
#let cells = data.at(0).map(strong) + data.slice(1).flatten()
#table(columns: data.at(0).len(), ..cells)

