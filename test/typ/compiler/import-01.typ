// An item import.
#import "module.typ": item
#test(item(1, 2), 3)

// Code mode
{
  import "module.typ": b
  test(b, 1)
}

// A wildcard import.
#import "module.typ": *

// It exists now!
#test(d, 3)

