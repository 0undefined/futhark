-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [3, 7, 12, 18, 25, 33, 42]
-- }
fun []int main([]int a) =
  let b = scan(+, 0, map(+2,a)) in
  b
