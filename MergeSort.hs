-- MergeSort.hs

module MergeSort where


-- based on Rabhi, Lapalme, Algorithms: A functional programming approach


-- MERGESORT
-- This version requires the input to be an Ord type, and uses the <= operator to do the comparisons

split :: [a] -> [[a]]
split [] = []
split (x:xs) = [x] : split xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys) =
	if x <= y
	then x : (merge xs b)
	else y : (merge a ys)

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs [] = []
mergePairs x@[l] = x -- ie [l]
mergePairs (l1:l2:ls) = ((:) $! (merge l1 l2)) (mergePairs ls)

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = ms (split xs)
	where
		ms [] = []
		ms [x] = x
		ms xs = ms (mergePairs xs)


-- MERGESORT TAKING COMPARISON FUNCTION
-- This version takes the comparison operator as an input

merge' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge' _ [] b = b
merge' _ a [] = a
merge' before a@(x:xs) b@(y:ys) =
	if x `before` y
	then x : (merge' before xs b)
	else y : (merge' before a ys)

mergePairs' :: (a -> a -> Bool) -> [[a]] -> [[a]]
mergePairs' _ [] = []
mergePairs' _ x@[l] = x -- ie [l]
mergePairs' before (l1:l2:ls) = ((:) $! (merge' before l1 l2)) (mergePairs' before ls)

mergeSort' :: (a -> a -> Bool) -> [a] -> [a]
mergeSort' before xs = ms' (split xs)
	where
		ms' [] = []
		ms' [x] = x
		ms' xs = ms' (mergePairs' before xs)
