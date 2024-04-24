esPar :: Int -> Bool
esPar n
    | n `mod`2 == 0 = True
    | otherwise = False
    
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

unzip' :: [(a,b)] -> ([a],[b])
unzip' xs = foldr (\(x, y) (accX, accY) -> (x : accX, y : accY)) ([],[]) xs

pair2List :: (a, [b]) -> [(a, b)]
pair2List (x, xs) = foldr (\y acc -> (x, y) : acc) [] xs

maxP :: (Int, Int) -> Int
maxP (x, y) = if (x > y) then x else y

minP :: (Int, Int) -> Int
minP (x, y) = if (x < y) then x else y

maxL :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxL (x, y) (a, b)
    | (maxP (x, y) - minP (x, y)) > (maxP (a, b) - minP (a, b)) = (x, y)
    | otherwise = (a, b)

maxSec :: [(Int, Int)] -> (Int, Int)
maxSec xs = foldr maxL (head xs) (tail xs)
