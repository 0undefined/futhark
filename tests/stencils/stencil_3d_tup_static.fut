-- Testing for static stencil index for tuples in 3D
-- ==
--
-- input {
-- [[[0u32, 5u32, 10u32, 15u32],
--   [50u32, 55u32, 60u32, 65u32],
--   [100u32, 105u32, 110u32, 115u32]],
--  [[500u32, 505u32, 510u32, 515u32],
--   [550u32, 555u32, 560u32, 565u32],
--   [600u32, 605u32, 610u32, 615u32]]]
--
-- [[[0u64, 3u64, 6u64, 9u64],
--   [30u64, 33u64, 36u64, 39u64],
--   [60u64, 63u64, 66u64, 69u64]],
--  [[300u64, 303u64, 306u64, 309u64],
--   [330u64, 333u64, 336u64, 339u64],
--   [360u64, 363u64, 366u64, 369u64]]]
--
-- [[[0u8, 1u8, 2u8, 3u8],
--  [10u8, 11u8, 12u8, 13u8],
--  [20u8, 21u8, 22u8, 23u8]],
--  [[100u8, 101u8, 102u8, 103u8],
--   [110u8, 111u8, 112u8, 113u8],
--   [120u8, 121u8, 122u8, 123u8]]]
-- }
-- output {
-- [[[24u64, 63u64, 1046u64, 1213u64],
--   [126u64, 309u64, 1100u64, 1219u64],
--   [1012u64, 43u64, 242u64, 505u64]],
--  [[836u64, 251u64, 1874u64, 1977u64],
--   [890u64, 113u64, 1864u64, 2047u64],
--   [1600u64, 279u64, 430u64, 661u64]]]
-- }
-- compiled random input { [33][65][65]u32 [33][65][65]u64 [33][65][65]u8 } auto output


let main [n][m][k] (xsss : [n][m][k]u32) (ysss : [n][m][k]u64) (zsss : [n][m][k]u8) : [n][m][k]u64 =
  let f (xm:u32,ym:u64,zm:u8) (xyzs : [8](u32,u64,u8)) : u64 =
    let (xs,ys,zs) = unzip3 xyzs
    let rx = ((((((((xs[0] ^ xs[1]) + xs[2]) ^ xs[3]) + xs[4]) ^ xs[5]) + xs[6]) ^ xs[7]) + xm)
    let ry = ((((((((ys[0] ^ ys[1]) + ys[2]) ^ ys[3]) + ys[4]) ^ ys[5]) + ys[6]) ^ ys[7]) + ym)
    let rz = ((((((((zs[0] ^ zs[1]) + zs[2]) ^ zs[3]) + zs[4]) ^ zs[5]) + zs[6]) ^ zs[7]) + zm)
    in (u64.u32 rx) ^ ry ^ (u64.u8 rz)
  let ixs = [(-1,-1,-1), (-1,-1,1), (-1,1,-1), (-1,1,1), (1,-1,-1), (1,-1,1), (1,1,-1), (1,1,1)]
  let tzip3 (x,y,z) = zip3 x y z
  let arr = zip3 xsss ysss zsss |> map tzip3 |> map (map tzip3)
  in stencil_3d ixs f arr arr
