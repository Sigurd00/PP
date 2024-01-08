
-- Before session
data Onion a = Core a | Layer (Onion a) deriving Show

instance Functor Onion where
    fmap f (Core a) = Core (f a)
    fmap f (Layer onion) = Layer (fmap f onion)

-- Problem 1
data UTree a = Node a [UTree a]

instance Functor UTree where
    fmap f (Node x []) = Node (f x) []  -- Base case: Node with no subtrees
    fmap f (Node x subtrees) = Node (f x) (map (fmap f) subtrees)  -- Recursive case: Node with subtrees

-- Problem 2

--instance Functor ((->) r) where
--    fmap f g = \x -> f (g x)

-- Problem 3
--(<*>) :: [a -> b] -> [a] -> [b]
--fs <*> xs = concat (fmap (\f -> fmap f xs) fs)


prodthree xs ys zs = (*) <$> xs <*> ys <*> zs
-- Explanation:
    --(*) <$> xs <*> ys <*> zs applies the (*) function (multiplication) to all possible combinations of triples from the input lists xs, ys, and zs.
    --The <$> operator (fmap) lifts the multiplication function (*) to operate on each element of xs.
    --The <*> operator performs the Cartesian product of the lists xs, ys, and zs, resulting in all possible combinations of triples.
    --The result is a list containing the products of all the triples formed from elements of the input lists.

    --For example, prodthree [1,2,3] [4,5,6] [7,8,9] will generate the list of all products of triples as described.
