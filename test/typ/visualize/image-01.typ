// Test configuring the size and fitting behaviour of images.

// Set width and height explicitly.
#box(image("test/assets/files/rhino.png", width: 30pt))
#box(image("test/assets/files/rhino.png", height: 30pt))

// Set width and height explicitly and force stretching.
#image("test/assets/files/monkey.svg", width: 100%, height: 20pt, fit: "stretch")

// Make sure the bounding-box of the image is correct.
#align(bottom + right, image("test/assets/files/tiger.jpg", width: 40pt, alt: "A tiger"))

