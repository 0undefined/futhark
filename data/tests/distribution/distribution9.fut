-- Test that we sequentialise the distributed kernels.  Currently done
-- by never having parallel constructs inside branches.  If we ever
-- start doing something clever with branches, this test may have to
-- be revised.
--
-- ==
-- structure distributed { If/Kernel 0 }

fun [[int]] main([[int]] a) =
  map(fn [int] ([int] a_r) =>
        if a_r[0] > 0
        then map(*2, a_r)
        else map(*3, a_r)
     , a)
