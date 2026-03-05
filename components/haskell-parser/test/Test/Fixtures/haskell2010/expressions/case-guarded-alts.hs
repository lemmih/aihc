module ExprS313CaseGuardedAlts where
x n = case n of { m | m > 0 -> m; _ -> 0 }
