module ExprS317PatAsPattern where
x xs = case xs of { ys@(y:_) -> y; [] -> 0 }
