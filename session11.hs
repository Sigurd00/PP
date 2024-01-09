tuple :: Monad m => m a -> m b -> m (a, b)
tuple ma mb = ma >>= \a -> mb >>= \b -> return (a, b)

tuple' :: Monad m => m a -> m b -> m (a, b)
tuple' ma mb = do
    a <- ma
    b <- mb
    return (a, b)

--do
--  y <- z
--  s y
--  return (f y)

--z >>= (\y -> s y >>= \_ -> return (f y))


fourfirst xs = do
    x <- xs
    return (4,x)
-- It uses the list to create a list of pairs of 4 followed by the element in the list [1,2,3] -> [(4,1), (4,2), (4,3)]

data W x = Bingo x deriving Show

instance Functor W where
    fmap f (Bingo x) = Bingo (f x)

instance Applicative W where
    pure x = Bingo x
    (Bingo f) <*> (Bingo x) = Bingo (f x)

instance Monad W where
    return x = Bingo x
    Bingo x >>= f = f x

wrapadd :: Num b => b -> W b -> W b
wrapadd x (Bingo y) = do
    return (x + y)

h (Bingo x) (Bingo y) = do
    return Bingo (x * y)

data Tree a = Leaf a | Node (Tree a) (Tree a)

minmax :: Ord a => Tree a -> Maybe (a, a)
minmax (Leaf x) = Just (x, x)
minmax (Node left right) = do
    (minL, maxL) <- minmax left
    (minR, maxR) <- minmax right
    let minVal = min minL minR
    let maxVal = max maxL maxR
    if maxL <= minR
        then Just (minVal, maxVal)
        else Nothing

minorder :: Ord a => Tree a -> Maybe a
minorder tree = do
    (minVal, maxVal) <- minmax tree
    if minVal <= maxVal
        then Just minVal
        else Nothing


foldM :: Monad m => (t1 -> t2 -> m t2) -> [t1] -> t2 -> m t2
foldM _ [] acc = return acc
foldM f (x:xs) acc = do
    newAcc <- f x acc
    foldM f xs newAcc

dingo :: Int -> IO Int
dingo x = do
    print x
    return x


main = do
    -- Test cases for minmax function
    let testMinMaxOrderedTree = minmax (Node (Leaf 1) (Leaf 3))
    let testMinMaxUnorderedTree = minmax (Node (Leaf 5) (Leaf 3))
    -- Test cases for minorder function
    let testMinOrderOrderedTree = minorder (Node (Leaf 1) (Leaf 3))
    let testMinOrderUnorderedTree = minorder (Node (Leaf 5) (Leaf 3))
    let testMinOrderIdenticalValuesTree = minorder (Node (Leaf 2) (Leaf 2))

    _ <- foldM (\x y -> dingo (x + y)) [1, 2, 3, 4] 0 -- should return 1, 3, 6, 10
    
    print testMinMaxOrderedTree           -- Should return: Just (1, 3)
    print testMinMaxUnorderedTree         -- Should return: Nothing
    print testMinOrderOrderedTree         -- Should return: Just 1
    print testMinOrderUnorderedTree       -- Should return: Nothing
    print testMinOrderIdenticalValuesTree -- Should return: Just 2
