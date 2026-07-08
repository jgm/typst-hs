// https://github.com/jgm/typst-hs/issues/100
// Settable fields like marker should be accessible in a show rule
// even when not explicitly set on the element.
#let checklist(body) = {
  show list: it => {
    let default-marker = if type(it.marker) == array {
      it.marker.at(0)
    } else {
      it.marker
    }
    [(#it.tight, #it.indent, #it.body-indent, #it.spacing, #default-marker)]
  }
  body
}
#show: checklist
- [ ] meow
