-- Does the sin64 function work?
-- ==
-- input { 0.0 } output { 0.0 }
-- input { -1.0 } output { -0.84147096 }
-- input { 3.1415927 } output { -8.742278e-8 }
-- input { -3.1415927 } output { 8.742278e-8 }

fun f64 main(f64 x) = sin64(x)
