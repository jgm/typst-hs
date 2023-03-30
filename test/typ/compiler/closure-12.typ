// Named arguments.
#{
  let greet(name, birthday: false) = {
    if birthday { "Happy Birthday, " } else { "Hey, " } + name + "!"
  }

  test(greet("Typst"), "Hey, Typst!")
  test(greet("Typst", birthday: true), "Happy Birthday, Typst!")

  // Error: 23-35 unexpected argument: whatever
  test(greet("Typst", whatever: 10))
}

