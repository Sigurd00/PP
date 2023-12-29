data Unary = Z | I Unary

unary2int :: Unary -> Integer
unary2int Z     = 0
unary2int (I n) = 1 + unary2int n 


data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
    deriving (Show)

least :: Ord a => Tree a -> a
least (Leaf x) = x
least (Node left right) = min (least left) (least right)


data Aexp = Num Int          -- Numeral (Integer)
          | Var String       -- Variable (String)
          | Add Aexp Aexp    -- Addition expression
          | Mul Aexp Aexp    -- Multiplication expression
    deriving (Show)


-- Test cases
main :: IO ()
main = do
    let unaryNum = I (I (I (I Z)))  -- Represents the unary numeral IIIIZ

    putStrLn $ "Unary to Integer: " ++ show (unary2int unaryNum) -- Output: Unary to Integer: 4

    let sampleTree = Node (Node (Leaf 5)
               (Leaf 3))
         (Node (Leaf 8)
               (Leaf 2))
    putStrLn $ "Least node in tree: " ++ show (least sampleTree) -- Output: Least node in tree: 2