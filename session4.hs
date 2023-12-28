onlytwo :: [a] -> Bool
onlytwo [_, _] = True
onlytwo _ = False


alldots :: Num a => [(a,a)] -> [(a,a)] -> [a]
alldots xs ys = [a * c + b * d | (a,b) <- xs, (c,d) <- ys]
-- tests for alldots
-- list1 = [(1, 2), (3, 4)]
-- list2 = [(5, 6), (7, 8)]
-- Output: [17,23,39,53]

seven :: Integral a => a -> [a]
seven k = [x | x <- [1..k], x `mod` 7 == 0]

pyt :: (Num a, Ord a, Enum a) => a -> [(a, a, a)]
pyt k = [(a, b, c) |
           a <- [0 .. k],
           b <- [0 .. k],
           a <= b,
           c <- [0 .. k],
           b < c,
           a ^ 2 + b ^ 2 == c ^ 2]

-- headsup type should be Eq because it should be able to be applied on any type that can be compared
-- we can use pattern matching or guarded equations to solve it instead.
headsup :: Eq a => [a] -> Bool
headsup [] = False
headsup [x1] = False
headsup (x1:x2:_) = x1 == x2

-- plonk z x y = x + y + z
plonk :: Num a => a -> a -> a -> a
plonk z = \x -> (\y -> ( x + y + z))


exampleExpression :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
exampleExpression _ _ (x, y) = if x == y then x else y

flop :: [(a, b)] -> [(b, a)]
flop xs = [(b, a) | (a, b) <- xs]

dupli :: [a] -> [a]
dupli xs = concat [[x, x] | x <- xs]

factors :: Int -> [Int]
factors n
    | n <= 0    = error "Input must be a positive integer"
    | otherwise = [x | x <- [1..n], n `mod` x == 0]

isperfect :: Int -> Bool
isperfect n = sum ( factors n) - n == n


bighead :: Ord a => [a] -> Int
bighead [] = 0
bighead (x:xs) = length [y | y <- xs, y > x]

--sums m n = [ x+y | x <- [1..m], y <- [ 1..n ]]

sums m n = concat [[x + y | y <- [1..n]] | x <- [1..m]]