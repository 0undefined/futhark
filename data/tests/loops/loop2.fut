-- A loop that doesn't involve in-place updates or even arrays.
-- ==
-- input {
--   42
-- }
-- output {
--   861
-- }

fun int main(int n) =
  loop (x = 0) = for i < n do x + i in
  x
