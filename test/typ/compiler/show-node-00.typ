// Override lists.
#show list: it => "(" + it.children.map(v => v.body).join(", ") + ")"

- A
  - B
  - C
- D
- E

