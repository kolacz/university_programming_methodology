nat2 :: [(Integer,Integer)]
nat2 = [(y,x-y) | x <-[0..], y<-[0..x]]

halve :: [a] -> ([a],[a])
halve [] = ([], [])
halve (x:xs) = 

merge :: Ord a => ([a], [a]) -> [a]
merge (xs, []) = xs
merge ([], xs) = xs
merge (x:xs, y:ys) = 
    | x < y -> x : merge (xs, y:ys)
    | otherwise -> y : merge (x:xs, ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge . cross (msort,msort) . halve $ xs