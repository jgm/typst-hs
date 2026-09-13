// A negative start index beyond the array length must produce an error, not
// crash the evaluator with an out-of-range Data.Vector.slice exception.
#((1, 2, 3).slice(-100))
