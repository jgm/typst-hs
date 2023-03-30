// Test reading plain text files
#let data = read("test/assets/files/hello.txt")
#test(data, "Hello, world!")

