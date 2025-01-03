// Test reading TOML data.
#let data = toml("/assets/files/toml-types.toml")
#test(data.string, "wonderful")
#test(data.integer, 42)
#test(data.float, 3.14)
#test(data.boolean, true)
#test(data.date_time, "2023-02-01T15:38:57Z")
#test(data.array, (1, "string", 3.0, false))
#test(data.inline_table, ("first": "amazing", "second": "greater") )
#test(data.table.element, 5)
#test(data.table.others, (false, "indeed", 7))

