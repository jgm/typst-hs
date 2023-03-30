// Test conditional set.
#show ref: it => {
  set text(red) if it.target == <unknown>
  "@" + str(it.target)
}

@hello from the @unknown

