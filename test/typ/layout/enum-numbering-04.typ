// Test numbering with closure and nested lists.
#set text(font: "New Computer Modern")
#set enum(numbering: (..args) => math.mat(args.pos()), full: true)
+ A
  + B
  + C
    + D
+ E
+ F

