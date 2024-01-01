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

type Assignment = [(String, Int)]  -- Association list representing variable assignments

-- Function to evaluate an arithmetic expression given an assignment
eval :: Aexp -> Assignment -> Int
eval (Num n) _ = n  -- Base case: Return the numeral (integer) value
eval (Var x) ass = case lookup x ass of  -- Look up the value of the variable in the assignment
                    Just val -> val
                    Nothing -> error ("Variable " ++ x ++ " not found in the assignment")
eval (Add e1 e2) ass = eval e1 ass + eval e2 ass
eval (Mul e1 e2) ass = eval e1 ass * eval e2 ass

data STree a = SLeaf a
            | Empty
            | SNode (STree a) a (STree a)
    deriving (Show)

insert :: Ord a => STree a -> a -> STree a
insert Empty x  = SLeaf x                         -- If the tree is empty, create a leaf node with x
insert (SLeaf a) x
    | x < a = SNode (SLeaf x) a Empty             -- Insert x to the left if x is smaller
    | x > a = SNode Empty a (SLeaf x)             -- Insert x to the right if x is larger
    | otherwise = SLeaf a                         -- If x is equal to a, keep the same leaf node
insert (SNode left val right) x
    | x < val   = SNode (insert left x) val right -- Traverse left subtree for insertion
    | x > val   = SNode left val (insert right x) -- Traverse right subtree for insertion
    | otherwise = SNode left val right            -- If x is equal to val, maintain the tree


inorderTraversal :: STree a -> [a]
inorderTraversal Empty = []
inorderTraversal (SLeaf x) = [x]
inorderTraversal (SNode left val right) = inorderTraversal left ++ [val] ++ inorderTraversal right
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

    -- Sample assignment
    let assignment = [("x", 3), ("y", 4)]
    let testExpression = Add (Mul (Num 2) (Var "x")) (Var "y")
    putStrLn $ "Evaluation of expression: " ++ show (eval testExpression assignment) -- Output: Evaluation of expression: 10

    let sampleTree = SNode (SLeaf 5) 10 (SLeaf 15)
    let updatedTree1 = insert sampleTree 7  -- Insert 7 into the tree
    let testInorderTraversal1 = inorderTraversal updatedTree1
    let updatedTree2 = insert updatedTree1 3  -- Insert 3 into the tree
    let testInorderTraversal2 = inorderTraversal updatedTree2

    putStrLn $ "Traversal of tree 1: " ++ show testInorderTraversal1 -- Output: Traversal of tree 1: [5, 7, 10, 15]
    putStrLn $ "Traversal of tree 2: " ++ show testInorderTraversal2 -- Output: Traversal of tree 2: [3, 5, 7, 10, 15]

