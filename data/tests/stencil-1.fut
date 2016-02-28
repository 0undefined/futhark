-- Simple rank-1 one-dimensional stencil computation.  Eventually
-- smooths out all differences.
--
-- ==
-- input { 1 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] }
-- output { [1.3333333333333333, 2.0, 3.0, 3.9999999999999996, 5.0, 5.666666666666666] }
-- input { 2 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] }
-- output {
--   [1.5555555555555554,
--    2.111111111111111,
--    2.9999999999999996,
--    3.9999999999999996,
--    4.888888888888888,
--    5.444444444444444] }


fun [f64] main(int num_iterations, [f64,n] a) =
  loop (a) = for i < num_iterations do
    map(fn f64 (int i) =>
          let x = if i == 0 then a[i] else unsafe a[i-1] in
          let y = a[i] in
          let z = if i == n-1 then a[i] else unsafe a[i+1] in
          let factor = 1.0/3.0 in
          factor*x + factor*y + factor*z
       , iota(n)) in
  a
