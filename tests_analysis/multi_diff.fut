def main [l][n][m] (xsss: [l][n][m]i64) : [l]i64 =
    map2 (\xss i -> #[unsafe] xss[0,i]) xsss (iota l)

-- === Expected output of analysis:
-- entry_main
--   segmap_usable_groups_5383 []
--   lifted_lambda_res_5389 [[σ gtid_5385 | ν | par], [τ 0i64 | ψ ], [σ gtid_5385 | ν | par]]