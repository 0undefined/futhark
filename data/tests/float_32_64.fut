-- Test that float32s and float64s can both be used in a program.
--
-- This program does not really test their semantics, but mostly that
-- the parser permits them.
--
-- ==
-- input { 3.14 } output { 3.0 }

fun f32 main(f64 x) =
  f32(int(x))
