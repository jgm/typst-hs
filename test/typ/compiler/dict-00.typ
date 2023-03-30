// Ref: true

// Empty
#(:)

// Two pairs and string key.
#let dict = (normal: 1, "spacy key": 2)
#dict

#test(dict.normal, 1)
#test(dict.at("spacy key"), 2)

