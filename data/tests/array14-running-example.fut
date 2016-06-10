-- Example program from the ARRAY'14 paper.
-- ==
-- tags { no_opencl }

fun [[f64]] main(*[int] xs, *[[f64]] as) =
  let m = size(0,as) in
  let n = size(1,as) in
  map (fn *[f64] ((int, *[f64]) e) =>
         let (i, a) = e in
         loop(a) = for j < n do
           let a[j] = a[ xs[j] ] * 2.0 in a
         in
         map (fn f64 (int j) =>
                if (j < 2*i) && (xs[j] == j)
                then a[j*i] else 0.0
             ,iota(n))
      ,zip(iota(m), as) )
