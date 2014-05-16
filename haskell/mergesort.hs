mergesort :: (Ord a) => ([a], Int) -> [a]
mergesort ([], _)       = []
mergesort ([x], _)      = [x]
mergesort (l@(x:xs), n) =
  let m = n `div` 2
      (xs, ys) = splitAt m l
      a = mergesort (xs, m)
      b = mergesort (ys, n - m)
  in merge a b []

merge :: (Ord a) => [a] -> [a] -> [a] -> [a]
merge []  []  acc = acc
merge [x] [y] acc = if x <= y then acc ++ [x,y] else acc ++ [y,x]
merge []  l   acc = acc ++ l
merge l   []  acc = acc ++ l
merge l1@(x:xs) l2@(y:ys) acc
  | x <= y    = merge xs l2 (acc ++ [x])
  | otherwise = merge l1 ys (acc ++ [y])

main :: IO ()
main =
  let l = [8,7,6,5,4,3,2,1,0]
  in print $ mergesort (l, length l)
