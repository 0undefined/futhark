-- Map-reduce inside group.
-- ==
-- random input { [1][256]i32 } auto output
-- random input { [100][256]i32 } auto output
-- random input { [100][512]i32 } auto output
-- structure distributed { SegMap/SegRed 1 }

let main = map (map i32.abs >-> i32.sum)
