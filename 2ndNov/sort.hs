insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
	| (x<=y) = x:y:ys
	| (x>y) = y : (insert x ys)

insertion_sort :: [Int] -> [Int]
insertion_sort [] = []
insertion_sort (x:xs) = insert x (insertion_sort xs)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

merge_sort :: [Int] -> [Int]
merge_sort [] = []
merge_sort[x] = [x]
merge_sort l = merge (merge_sort (take (div (length l) 2) l)) (merge_sort (drop (div (length l) 2) l))

{-merge_sort :: [Int] -> [Int]
merge_sort [] = []
merge_sort (x:xs) = merge_sort
-}
main = do
	let l1 = [20,10..0]
	let l2 = []
	let l3 = [3,2,10,7,41,8,13]
	let l4 = [7.2,3.0,4.7,1.24,5.5,5.6]

	let l5 = [2,5,15,21]
	let l6 = [7,9,13,18]

	print $ l1
	print $ insertion_sort l1

	--take, drop inbuilt functions
	print $ take 3 l3--take 1st 3 elements of the list
	print $ drop 3 l3--drop 1st 3 elements from the list

	print $ merge l5 l6
	print $ merge_sort l3