-- Testing for dynamic stencil index in 3D
-- ==
--
-- input {[[[0i64, 1i64, 2i64, 3i64],
--          [10i64, 11i64, 12i64, 13i64],
--          [20i64, 21i64, 22i64, 23i64]],
--         [[100i64, 101i64, 102i64, 103i64],
--          [110i64, 111i64, 112i64, 113i64],
--          [120i64, 121i64, 122i64, 123i64]]]
--       [-1i64,0i64,1i64] 
--       }
-- output {   [[[111i64, 113i64, 116i64, 118i64],
--              [131i64, 133i64, 136i64, 138i64],
--              [151i64, 153i64, 156i64, 158i64]],
--             [[211i64, 213i64, 216i64, 218i64],
--              [231i64, 233i64, 236i64, 238i64],
--              [251i64, 253i64, 256i64, 258i64]]] 
--        }


let main [n][m][k] (arr : [n][m][k]i64) (range : [3]i64) : [n][m][k]i64  =
  let ixs = zip3 range range range
  let f _ xs = xs[0] + xs[1] + xs[2]
  in stencil_3d ixs f (map (map (map (const ()))) arr) arr
