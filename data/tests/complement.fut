// Test that complement works properly.
// --
// input {
//   [1, 255, 0]
// }
// output {
//   [-2, -256, -1]
// }
fun [int] main([int] a) =
    map(~, a)
