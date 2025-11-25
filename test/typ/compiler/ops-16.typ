// assignments with nested destructuring bind

#test({
    let a = 1
    let b = 2
    (a, b) = (b, a)
    (a, b)
  },
  (2, 1)
)

#test({
    let a = 1
    let b = 2
    (a, _) = (b, a)
    (a, b)
  },
  (2, 2)
)

#test({
  let a = 0
  let b = 0

  (_, (_, a), b) = (1, (2, 3), 4)

  (a, b)
},
(3, 4)
)

#test({
    let x = 0
    let y = 0
    let z = 0
    (x: (y, (x, z: (_, z)))) = (z: 1, x: (10, (x: 100, y: 200, z: (1000, 2000))))

    (x, y, z)
  },
  (100, 10, 2000)
)

#test({
    let a = (1, 2, 3)
    (x: (a.first(), (x: a.at(1), z: (_, a.last())))) = (z: 1, x: (10, (x: 100, y: 200, z: (1000, 2000))))

    a
  },
  (10, 100, 2000)
)

#test({
    let d = (x: 1, y: 2, z: 3)
    (x: (d.x, (x: d.y, z: (_, d.z)))) = (z: 1, x: (10, (x: 100, y: 200, z: (1000, 2000))))

    d
  },
  (x: 10, y: 100, z: 2000)
)

#test({
  let d = (x: 1, y: (-1, -2))
  let i = 1

  (..d.x, (_, (_, d.y.at(i)),_)) = (0, 1, (2, (3, 4), 5))
  
  d
},(x: (0, 1), y: (-1, 4)))

// assignment order: left to right

#test({
  let x = 0
  (x, x) = (1, 2)
  x
}, 2)

#test({
  let x = 0
  (.., x, x) = (1, 2)
  x
}, 2)

#test({
  let x = 0
  (x, ..x, x) = (1, 2)
  x
}, 2)

