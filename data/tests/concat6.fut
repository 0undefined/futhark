-- Concatenating across intermediate dimension.
--
-- ==
-- input { [[[1,2],[3,4],[5,6]]] [[[7,8]]] }
-- output { [[[1,2],[3,4],[5,6],[7,8]]] }
--
-- input { [[[1,2],[4,5],[7,8]]] [[[3,2],[6,5],[9,8]]] }
-- output { [[[1,2],[4,5],[7,8],[3,2],[6,5],[9,8]]] }

fun [n][][m]int main([n][][m]int xs, [n][][m]int ys) =
  concat@1(xs, ys)
