-- learn.hs

module Learn where
-- First, we declare the name of our module so
-- it can be imported by name in a project
-- We won't be doning a project of this size
-- for a while yet.

x = 10 * 5 + y

myResult = x * 5

y = 10

foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

