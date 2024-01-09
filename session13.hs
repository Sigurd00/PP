import Data.List (unfoldr)

nsonlyRec :: Int -> [Int]
nsonlyRec n = [n * x | x <- [0..]]

nsonly :: Int -> [Int]
nsonly n = map (* n) [0..]

-- Problem 1
-- The lazy evaluation only computes the necessary elements of x up to the point needed by the take function.

fibsfrom :: Integer -> Integer -> [Integer]
fibsfrom a b = a : b : zipWith (+) (fibsfrom a b) (tail (fibsfrom a b))

-- When attempting to compute fib 50 using fibsfrom, it will suffer from a similar issue as the direct recursive approach.
-- Although the fibsfrom function generates an infinite list efficiently in a lazy evaluation context, 
-- computing an element far along the infinite list (like fib 50) will require evaluating all the preceding elements. 
-- This becomes increasingly resource-intensive and impractical for such large calculations due to the cumulative 
-- computation of all previous Fibonacci numbers leading up to fib 50. Therefore, attempting to directly compute fib 50
-- using fibsfrom will still encounter performance issues similar to the recursive approach.

indflet :: a -> [a] -> [a]
indflet _ [] = []
indflet _ [x] = [x]
indflet e (x:y:ys) = x : e : indflet e (y:ys)


-- Helper function to convert an integer to its binary representation as a string
toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (head $ show $ x `mod` 2, x `div` 2)) n

-- Function to generate an infinite list of ordered binary numbers
allBinaries :: [String]
allBinaries = map toBinary [0..]

data Tree = Node Tree Tree | Leaf

data Direction = L | R  -- Left and Right

type Path = [Direction]

-- Function to generate all finite paths from the root to any leaf of the tree
allFinitePaths :: Tree -> [Path]
allFinitePaths t = generatePaths t []

-- Helper function to generate paths recursively
generatePaths :: Tree -> Path -> [Path]
generatePaths Leaf path = [reverse path]  -- Reached a leaf, return the reversed path (from root to leaf)
generatePaths (Node left right) path = 
    generatePaths left (L:path) ++ generatePaths right (R:path)
