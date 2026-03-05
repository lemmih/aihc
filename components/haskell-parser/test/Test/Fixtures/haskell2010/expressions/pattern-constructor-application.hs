module ExprS317PatConstructorApplication where
data Box = Box Int
x b = case b of { Box n -> n }
