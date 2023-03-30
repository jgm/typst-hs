// Test rvalue missing key.
#{
  let dict = (a: 1, b: 2)
  // Error: 11-23 dictionary does not contain key "c" and no default value was specified
  let x = dict.at("c")
}

