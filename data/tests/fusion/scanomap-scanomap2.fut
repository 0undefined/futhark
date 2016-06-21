-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [2, 3, 4, 5, 6, 7, 8]
--   [2, 5, 9, 14, 20, 27, 35]
--   [3, 4, 5, 6, 7, 8, 9]
--   [3, 12, 60, 360,2520,20160,181440]
-- }
-- structure {
--   Scanomap 1
-- }
fun ([]int, []int, []int, []int) main([]int inp) =
  let a = map(+1, inp)
  let b = scan(+, 0, a) in
  let c = map(+1, a)
  let d = scan(*, 1, c) in
  (a, b, c, d)
