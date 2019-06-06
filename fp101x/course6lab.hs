evens :: [Integer] -> [Integer]
evens [] = []
evens (x:xs)
    | even x = x : evens xs
    | otherwise = evens xs

squares :: Integer -> [Integer]
squares n = map (^2) [1..n]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

squares' :: Integer -> Integer -> [Integer]
squares' m n = map (^2) [n+1..n+m]

sumSquares' x = sum . uncurry squares' $ (x, x)

-- incomplete
coords :: Integer -> Integer -> [(Integer, Integer)]
