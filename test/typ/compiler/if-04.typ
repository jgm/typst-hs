// Condition must be boolean.
// If it isn't, neither branch is evaluated.
// Error: 5-14 expected boolean, found string
#if "a" + "b" { nope } else { nope }

