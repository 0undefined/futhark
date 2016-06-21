-- ==
-- input { 8 4 3 }
-- output {
-- [[[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]],
--  [[0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32],
--   [0i32, 1i32, 2i32]]]
-- }

fun [][][]int main(int n, int m, int l) =
  let a = replicate(n, replicate(m, iota(l)))
  in rotate(1, -1, a)
