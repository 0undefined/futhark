-- Distribute a redomap inside of a map.
--
-- One possible structure:
--
-- map
--   map
-- map
--   reduce
--
-- Currently expected structure:
--
-- map
--   loop
-- ==
--
-- structure distributed { MapKernel 1 Reduce 0 Map 0 DoLoop 1 }

fun []int main([][]int a) =
  map(fn int ([]int a_r) =>
        reduce(+, 0, map(+1, a_r)),
      a)
