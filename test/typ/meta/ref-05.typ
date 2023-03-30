
#show ref: it => {
  if it.element != none {
    if it.element.func() == text {
      let element = it.element
      "["
      element
      "]"
    } else if it.element.func() == underline {
      let element = it.element
      "{"
      element
      "}"
    } else {
      it
    }
  } else {
    it
  }
}

@txt

Ref something unreferable <txt>

@under
#underline[
Some underline text.
] <under>
