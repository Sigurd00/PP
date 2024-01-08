import Data.List (group, sort)

--What is wrong with the initial solution?
--Pattern Matching: The pattern matching in the function definition is incorrect for achieving the desired outcome.
--Base Case: The base case for an empty list is missing.
--Type Signature: The output type specified in the function signature does not match the desired output format.

triples :: Num a => [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples ((a, b, c):xs) = 
    let (as, bs, cs) = triples xs
    in (a:as, b:bs, c:cs)

cfrac :: RealFrac a => a -> Int -> [Int]
cfrac _ 0 = []
cfrac r n = let integerPart = floor r
                fractionalPart = r - fromIntegral integerPart
            in integerPart : cfrac (1 / fractionalPart) (n - 1)

class InVector a where
    (&&&) :: a -> a -> a -- Vector sum
    (***) :: a -> a -> Int -- Inner product

instance InVector Bool where
    (&&&) :: Bool -> Bool -> Bool
    True &&& True = True -- Vector sum: 'and' operation for Booleans
    _ &&& _ = False

    (***) :: Bool -> Bool -> Int
    True *** True = 1 -- Inner product: 'True' and 'True' result in 1
    _ *** _ = 0 -- Any other combination results in 0


frequencies :: String -> [(Char, Int)]
frequencies str =
    let grouped = group (sort str) -- Group characters and sort them
    in map (\x -> (head x, length x)) grouped -- Map each character group to a pair (char, frequency)
