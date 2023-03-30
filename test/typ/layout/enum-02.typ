// Test automatic numbering in summed content.
#for i in range(5) {
   [+ #numbering("I", 1 + i)]
}

