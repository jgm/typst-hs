
#show ref: it => {
  if it.element != none and it.element.func() == figure {
    let element = it.element
    "["
    element.supplement
    "-"
    str(element.counter.at(element.location()).at(0))
    "]"
    // it
  } else {
    it
  }
}

#figure(
  image("test/assets/files/cylinder.svg", height: 3cm),
  caption: [A sylinder.],
  supplement: "Fig",
) <fig1>

#figure(
  image("test/assets/files/tiger.jpg", height: 3cm),
  caption: [A tiger.],
  supplement: "Figg",
) <fig2>

#figure(
  $ A = 1 $,
  kind: "equation",
  supplement: "Equa",

) <eq1>
@fig1

@fig2

@eq1

