// Test custom operator.
$ op("myop", limits: #false)_(x:=1) x \
  op("myop", limits: #true)_(x:=1) x $

