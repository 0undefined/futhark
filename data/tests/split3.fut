-- A better name would be 'take'...
-- ==
-- input {
--   3
--   [5.0,1.0,2.0,5.0]
-- }
-- output {
--   [5.000000, 1.000000, 2.000000]
--   [5.000000]
-- }
fun {[real], [real]} main(int n, [real] a) =
  split( (n), a)
