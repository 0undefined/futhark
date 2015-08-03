-- Extraction from generic pricer.  Uses shape declarations in ways
-- that were at one point problematic.
--
-- ==
-- input {
--   3
--   [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]]
--   [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
-- }
-- output {
--   [[109.0, 140.0, 171.0],
--    [-109.0, -140.0, -171.0],
--    [0.0, 0.0, 0.0],
--    [0.0, 0.0, 0.0],
--    [0.0, 0.0, 0.0]]
-- }

fun [[real]] main(
              int                  num_und,
             [[int, num_dates],3]  bb_inds,
             [real]                arr_usz
) =
  let arr    = reshape( (num_dates*num_und), arr_usz ) in
  let bb_data= map(fn [real] ([int] row) =>
                        map(toFloat,row)
                  , bb_inds )   in
  let bb_mat = brownianBridge( num_und, bb_inds, bb_data, arr )
  in  bb_mat


fun [real] brownianBridgeDates (
                [[int, num_dates],3] bb_inds,
                [[real,num_dates],3] bb_data,
                 [real,num_dates]    gauss
            ) =
    let bi = bb_inds[0] in
    let li = bb_inds[1] in
    let ri = bb_inds[2] in
    let sd = bb_data[0] in
    let lw = bb_data[1] in
    let rw = bb_data[2] in

    let bbrow = copy(replicate(num_dates, 0.0))   in
    let bbrow[ bi[0]-1 ] = sd[0] * gauss[0]       in

    loop (bbrow) =
        for i < num_dates-1 do  -- use i+1 since i in 1 .. num_dates-1
            let j  = li[i+1] - 1 in
            let k  = ri[i+1] - 1 in
            let l  = bi[i+1] - 1 in

            let wk = bbrow [k  ] in
            let zi = gauss [i+1] in
            let tmp= rw[i+1] * wk + sd[i+1] * zi in

            let bbrow[ l ] = if( j == -1)
                             then tmp
                             else tmp + lw[i+1] * bbrow[j]
            in  bbrow

        -- This can be written as map-reduce, but it
        --   needs delayed arrays to be mapped nicely!
    in loop (bbrow) =
        for ii < num_dates-1 do
            let i = num_dates - (ii+1) in
            let bbrow[i] = bbrow[i] - bbrow[i-1]
            in  bbrow
       in bbrow

fun [[real]] brownianBridge (
                int                  num_und,
                [[int, num_dates],3] bb_inds,
                [[real,num_dates],3] bb_data,
                 [real]  gaussian_arr
            ) =
    let gauss2d  = reshape((num_dates,num_und), gaussian_arr) in
    let gauss2dT = transpose(gauss2d) in
      transpose(
        map( brownianBridgeDates(bb_inds, bb_data), gauss2dT )
      )
