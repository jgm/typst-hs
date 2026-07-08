// Test dictionary map and filter.
#let dict = (a: 1, b: -2, c: 3)
#test(dict.map(v => v * 10), (a: 10, b: -20, c: 30))
#test(dict.filter(v => v > 0), (a: 1, c: 3))
#test((:).map(v => v), (:))
#test((:).filter(v => true), (:))
