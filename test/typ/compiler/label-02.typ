// Test labelled text.
#show "t": it => {
  set text(blue) if it.has("label") and it.label == <last>
  it
}

This is a thing #[that <last>] happened.

