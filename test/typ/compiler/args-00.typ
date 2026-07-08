// Test map and filter on arguments.
#let collect(..args) = args
#let args = collect(1, 2, x: 3)
#let doubled = args.map(v => v * 2)
#test(doubled.pos(), (2, 4))
#test(doubled.named(), (x: 6))
#let filtered = args.filter(v => v > 1)
#test(filtered.pos(), (2,))
#test(filtered.named(), (x: 3))
#test(collect().map(v => v).pos(), ())
