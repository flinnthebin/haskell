our_length :: [a] -> Int
our_length [] = 0
our_length (x : xs) = 1 + our_length xs

our_drop :: Int -> [a] -> [a]
our_drop i [] = []
our_drop i (x : xs) = (our_drop (i - 1) xs)
