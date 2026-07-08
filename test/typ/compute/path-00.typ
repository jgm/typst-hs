// Test the path type.
#let p = path("/assets/files/hello.txt")
#test(type(p), path)
#test(type(p) == path, true)
// constructing a path from a path returns it unchanged
#test(path(p), p)
#test(p == path("/assets/files/hello.txt"), true)
#test(p == path("/assets/files/data.csv"), false)
// paths are accepted where file-path strings are accepted
#test(read(p), "Hello, world!")
#test(csv(path("/assets/files/data.csv")), csv("/assets/files/data.csv"))
#image(path("/assets/files/rhino.png"))
