// Test reading XML data.
#let data = xml("test/assets/files/data.xml")
#test(data, ((
  tag: "data",
  attrs: (:),
  children: (
    "\n  ",
    (tag: "hello", attrs: (name: "hi"), children: ("1",)),
    "\n  ",
    (
      tag: "data",
      attrs: (:),
      children: (
        "\n    ",
        (tag: "hello", attrs: (:), children: ("World",)),
        "\n    ",
        (tag: "hello", attrs: (:), children: ("World",)),
        "\n  ",
      ),
    ),
    "\n",
  ),
),))

