module Utils where

replace :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replace _ _ [] = []
replace f r (x:xs)
  | f x == True = r x : replace f r xs
  | otherwise = x : replace f r xs
