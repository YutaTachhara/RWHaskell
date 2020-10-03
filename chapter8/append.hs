(++) :: [a] -> [a] -> [a]

(x:xs) ++ ys = y : (xs ++ ys)
[] ++ ys     = ys

