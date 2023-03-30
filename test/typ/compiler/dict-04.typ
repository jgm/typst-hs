// Missing lvalue is not automatically none-initialized.
#{
  let dict = (:)
  // Error: 3-9 dictionary does not contain key "b" and no default value was specified
  dict.b += 1
}

