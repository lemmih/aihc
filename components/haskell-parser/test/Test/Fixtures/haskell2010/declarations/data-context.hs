module D12 where
data Eq a => Set a = NilSet | ConsSet a (Set a)
