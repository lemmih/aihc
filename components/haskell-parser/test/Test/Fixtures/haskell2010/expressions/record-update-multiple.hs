module ExprS315RecordUpdateMultiple where
data R = R { a :: Int, b :: Int }
x r = r { a = 3, b = 4 }
