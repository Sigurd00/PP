replicatee :: Int -> Int -> [Int]
replicatee _ 0 = []
replicatee x n = x : replicatee x (n - 1)



improve :: [a] -> [a]
improve []           = []  -- Handling the case of an empty list
improve [x]          = [x] -- Handling the case of a list with only one element
improve (x : _ : xs) = x : improve xs


rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

mylast :: [a] -> a
mylast [x]    = x
mylast (_:xs) = mylast xs

isolate :: Eq a => [a] -> a -> ([a], [a])
isolate [] _ = ([], []) 
isolate (y:ys) x
    | y == x    = let (l1, l2) = isolate ys x in (l1, y : l2) -- if y == x then y goes into list l2
    | otherwise = let (l1, l2) = isolate ys x in (y : l1, l2) -- otherwise y goes into list l1

wrapup :: Eq a =>