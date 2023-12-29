positions :: String -> [Int]
positions str = map (\c -> fromEnum c - 96) str

sumsq :: Int -> Int
sumsq n = foldr (+) 0 [x^2 | x <- [1..n]]

within :: [Int] -> (Int, Int) -> [Int]
within xs (low, high) = filter (\x -> x >= low && x <= high) xs

sumrows :: [[Int]] -> [Int]
sumrows xss = map (foldr (+) 0) xss -- Every list is mapped to a foldr where we apply (+) for every element in that list

fact k = product [1..k]

approx :: Int -> Double
approx n = (foldr (+) 0) ( take n (map (\k -> 1 / fact k) [0..]))

fingo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys -- It constructs a list of ys + xs by using cons on each element thereby getting y1:y2..x1:x2

-- What is the type of map map?
-- We know that map has the type (a -> b) -> [a] -> [b].
-- (a -> b) means a function with parameter type a and return type b and [a] -> [b] means the list elements goes from type a to type b
-- When we have two maps we have (a -> b) -> (b -> c) -> [a] -> [c] Which means that the first funcion takes type a, returns type b which the 2nd function takes in and returns type c.
-- hmm haskell interpreter says map map :: [a -> b] -> [[a] -> [b]]
