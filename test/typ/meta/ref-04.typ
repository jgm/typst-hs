#set heading(numbering: (..nums) => {
  nums.pos().map(str).join(".")
  }, supplement: [Chapt])

#show ref: it => {
  if it.element != none and it.element.func() == heading {
    let element = it.element
    "["
    emph(element.supplement)
    "-"
    numbering(element.numbering, ..counter(heading).at(element.location()))
    "]"
  } else {
    it
  }
}

= Introduction <intro>

= Summary <sum>

== Subsection <sub>

@intro

@sum

@sub

