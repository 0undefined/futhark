-- Map returning an array predicated on the index variable.
--
-- ==
-- input { 2 }
-- output { [0, 1] }

fun [[int]] main(int chunk) =
  map( fn [int,1] (int k) =>
         if k==0 then [0] else [1]
     , iota(chunk))
