-- Name: Jacob Johannes Sigurd Skadborg
-- AAU mail address: jskadb20@student.aau.dk
-- Study number: 20203111


-- PROBLEM 1

-- 1.1
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
-- Rotate is parametric polymorphism because type variable 'a' can be any type.

-- 1.2
allrotates :: [a] -> [[a]]
allrotates [] = []
allrotates (x:xs) = (x:xs) : allrotates (rotate (x:xs))
-- allrotates is parametric polymorphism because type variable 'a' can be any type.
-- This never terminates :(
-- 1.3
allrotates' :: [a] -> [[a]]
allrotates' [] = []
allrotates' (x:xs) = map rotate ((x:xs) : allrotates' (rotate (x:xs)))
-- This also never terminates :(
-- I guess i could add a guard that if x == the rotated list then []
-- This would work because that condition would tell us that we have rotated the list completely
-- But i just got this idea at the end of the exam and i dont have time to implement it.
-- PROBLEM 2

-- 2.1
data Tree a = Leaf a | UNode (Tree a) (Tree a) | LNode a (Tree a) (Tree a)
    deriving (Show)
t1 = UNode (UNode (Leaf 17) (Leaf 48400)) (Leaf 1964)
t2 = LNode "bingo" (Leaf "plip") (LNode "plop" (Leaf "uhu") (Leaf "fedtmule"))

-- UNode is an unlabeled node
-- LNode is a labeled node

-- 2.2
isfull :: Tree a -> Bool
isfull (Leaf a) = True
isfull (UNode l r) = False
isfull (LNode a l r) = isfull l && isfull r

-- Base cases are:
    -- if Leaf then true
    -- of unlabeled node then false
-- Recursive step:
    -- If we have a labeled node then see if the subtrees l and r are also fully labeled.
-- 2.3

--preorder :: Tree a -> Maybe [a]
--preorder (Leaf a) = Just [a]
--preorder (UNode l r ) = Nothing
--preorder (LNode a l r) = do
--                            left <- preorder l
--                            right <- preorder r
--                            label <- Just a
--                            return Just ([label] ++ [left] ++ [right])
-- This is not right so i have commented it out
-- PROBLEM 3

-- 3.1
remove xs ys = [y | y <- ys, not (elem y xs)]

-- 3.2
remove' :: Eq a => [a] -> [a] -> [a]
remove' [] ys = ys
remove' (x:xs) ys = remove' xs (filter (/=x) ys)

-- PROBLEM 4
newtype WrapString a = WS (a,String) deriving Show

instance Functor WrapString where
    fmap f (WS (x,s)) = WS (f x,s)

-- 4.1
instance Applicative WrapString where

    pure x = WS (x,"")
    (WS (f,s)) <*> (WS (x,s')) = WS (f x,s ++ s')

-- 4.2
instance Monad WrapString where
    return x = WS (x,"")
    (WS (x,s)) >>= f = let (WS (y,s')) = f x in WS (y,s ++ s')

-- 4.3

-- 5.1
fx :: (Ord a, Num a) => a -> a -> a -> (a, a)
fx x y z = if x < y && y < z - 1 then (x,z) else (y,z)
-- This function has ad hoc polymorphism because of the class constrains on type variable 'a'

-- 5.2
fx' :: [(Integer, p -> Char)]
fx' = [(123, \_ -> 'a')]
-- This function does not have polymorphism as there are no input parameters

-- 5.3
fx'' f x y = f x True
-- This function uses parametric polymorphism because type variables 't1' and 't2' can have any type

-- 5.4
fx''' :: (Num a, Enum a) => [a]
fx''' = [1..10]
-- This function uses ad hoc polymorphism because of the class constraints 'Num' and 'Enum' on type variable 'a'

-- PROBLEM 6

-- 6.1
-- Recursive definition of natural numbers
naturals :: [Integer]
naturals = 1 : map (+1) naturals

-- 6.2
facs :: [Integer]
facs = 1 : map (\x -> x * (last facs)) naturals

-- 6.3
facs' :: [Integer]
facs' = 1 : zipWith (*) facs' naturals
