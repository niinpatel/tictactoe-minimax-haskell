module Utils where

replace :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replace _ _ [] = []
replace f r (x:xs)
  | f x == True = r x : replace f r xs
  | otherwise = x : replace f r xs

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

scaleBy :: Num a => a -> (a, a) -> (a, a)
scaleBy scalar = mapPair (* scalar)

shiftBy :: Num a => a -> (a, a) -> (a, a)
shiftBy scalar = mapPair (+ scalar)

shrinkBy :: Fractional a => a -> (a, a) -> (a, a)
shrinkBy scalar = scaleBy (1 / scalar)
