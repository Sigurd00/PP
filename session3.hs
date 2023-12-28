twice f x = f (f x)
twicetwo (f, x) = f ( f x)

-- Because as we can see in the function the tuple (x,y) will be put into a list, and as we know a list contains elements of the same type, 
-- therefore the elements in the tuple has to be of the same type.
dingo :: (a, a) -> [a]
dingo ( x , y ) = [ x , y ]

-- This is currying. We can now use the function to only eat one parameter at a time
-- The type also has to be of the class Num because of the numerical operation *
mango :: Num a => a -> a -> a -> a
mango x y z = x * y + z - 42


bingo :: a -> a
bingo x = x

-- We know that the elements needs to be able to be compared, therefore Eq. 
-- We know that it is a list of tuples and it returns a list of tuples.
-- We also know that the pairs are the same type because they need to be compared
thesame :: Eq a => [(a, a)] -> [(a, a)]
thesame xs = filter (\(x, y) -> x == y) xs
--thesame = filter (\(x, y) -> x == y)
--thesame = filter (uncurry (==))
