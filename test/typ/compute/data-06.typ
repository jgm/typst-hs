// Test reading JSON data.
#let data = json("test/assets/files/zoo.json")
#test(data.len(), 3)
#test(data.at(0).name, "Debby")
#test(data.at(2).weight, 150)

