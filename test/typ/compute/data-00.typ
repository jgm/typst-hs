// Test reading plain text files
#let data = read("/assets/files/hello.txt")
#test(data, "Hello, world!")

