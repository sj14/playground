import Data.Char

let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

which n
    | n == 0 = "zero!"
    | even n = "even!"
    | otherwise = "odd!"

myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]
map (*2) [1..5] -- [2, 4, 6, 8, 10]
