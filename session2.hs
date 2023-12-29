second xs = xs !! 1

allbutsecond (x:_:xs) = x : xs

midtover xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
midtover2 xs = splitAt (length xs `div` 2) xs
twoLists xs = (xs,xs)

bingo x y = x `mod` z
    where
    z = y + 42

final xs = head (reverse xs)