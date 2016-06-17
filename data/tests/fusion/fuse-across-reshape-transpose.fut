-- ==
-- input {
-- }
-- output {
--   [[2, 8, 14], [4, 10, 16], [6, 12, 18]]
-- }
-- structure { Map 2 }
fun [][]int main() =
  let n = 9 in
  let a = map(+1,iota(n)) in
  let b = reshape((3,3),a) in
  let c = transpose(b) in
  map (fn []int ([]int row) =>
         map (*2, row),
       c)
