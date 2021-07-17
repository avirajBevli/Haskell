new_length :: [Int] -> Int
new_length [] = 0
new_length (x:xs) = 1 + new_length xs 

new_head :: [Int] -> Int
new_head [] = error "Empty list given as input!"
new_head (x:xs) = x

new_tail :: [Int] -> [Int]
new_tail [] = []
new_tail (x:xs) = xs

new_reverse :: [Int] -> [Int]
new_reverse [] = []
new_reverse (x:xs) = new_reverse xs ++ [x]

new_access :: [Int] -> Int -> Int
new_access [] a = error "Index out of bounds"
new_access (x:xs) 0 = x
new_access (x:xs) a = new_access xs (a-1)

new_sum :: [Int] -> Int
new_sum [] = 0
new_sum (x:xs) = x + new_sum xs


--Usage of Templating 
find_to_list :: Int -> [a] -> [a]
find_to_list index [] = [] --error "Index out of bounds"
find_to_list 0 (x:xs) = [x] 
find_to_list index (x:xs) = [x] ++ (find_to_list (index-1) xs)

find_from_list :: Int -> [a] -> [a]
find_from_list index [] = [] --error "Index out of bounds"
find_from_list 0 (x:xs) = xs 
find_from_list index (x:xs) = find_from_list (index-1) xs

--follows zero based indexing compared to the one based indexing followed by the inbuilt function splitAt
new_splitAt :: Int -> [a] -> ([a],[a])
new_splitAt index [] = error "Index out of bounds"
new_splitAt index list = ((find_to_list index list),(find_from_list index list))

is_bignum :: Int -> Bool
is_bignum n = (n >= 10)


--Templating for ordered data types
insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs)
	| (y<=x) = [y]++[x]++xs --or y:x:xs
	| (y>x) = [x]++(insert y xs)--or x:(insert y xs)

insertion_sort :: Ord a => [a] -> [a]
insertion_sort [] = []
insertion_sort (x:xs) = insert x (insertion_sort xs) 
-- insert x into the sorted array formed from xs to form the new full sorted array


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) 
	|(x<=y) = x:(merge xs (y:ys))
	|otherwise = y:(merge (x:xs) ys)

merge_sort :: Ord a => [a] -> [a]
merge_sort [] = []
merge_sort (x:[]) = [x]
--merge_sort list = merge (merge_sort (find_to_list (div (length list) 2) list)) (merge_sort (find_from_list (div (length list) 2) list))
merge_sort l = merge (merge_sort (take (div (length l) 2) l)) (merge_sort (drop (div (length l) 2) l))


smaller_elems :: Ord a => a -> [a] -> [a]
smaller_elems key [] = []
smaller_elems key (x:xs)
	| (x<=key) = x:(smaller_elems key xs)--we take equal to sign as well to handle arrays with repitition
	| (x>key) = smaller_elems key xs

--List of elements in the list which are bigger than a particular value(key)
bigger_elems :: Ord a => a -> [a] -> [a]
bigger_elems key [] = []
bigger_elems key (x:xs)
	| (x>key) = x:(bigger_elems key xs)--we do not take equal to sign here to avoid a single entry being considered more than once in the sorted list
	| (x<=key) = bigger_elems key xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort (smaller_elems x xs)) ++ [x] ++ (quicksort (bigger_elems x xs))

--Compact using list compression
quicksort_compact :: Ord a => [a] -> [a]  
quicksort_compact [] = [] 
quicksort_compact (x:xs) = (quicksort_compact [y | y <- xs, y <= x]) ++ [x] ++ (quicksort_compact [y | y <- xs, y > x])

are_two_cube_sum_same :: Int -> [(Int, Int)]
--We are only taking b only from 1 to (n-a^3) because going till n is a waste as for b>(n-a^3), it is impossible to obtain a^3+b^3 equal to n
--If we take b from 1 to n, computation will take a long time
are_two_cube_sum_same n
    | ( length( [(a,b) | a <- [1..n], b <- [1..n-a*a*a], a<b, (a*a*a+b*b*b)==n] ) <= 1 ) = []
    | otherwise = [(a,b) | a<-[1..n], b<-[1..n-a*a*a], a<b, (a*a*a+b*b*b)==n]

main = do
	let l1 = [1,3..17]
	print $ l1
	{-print $ l1
	print $ length l1
	print $ new_length l1
	print $ head l1
	print $ new_head l1
	print $ tail l1
	print $ new_tail l1
	print $ reverse l1
	--print $ access l1 4 --this function does not exist in haskell!!??
	print $ new_access l1 4
	print $ sum l1
	print $ new_sum l1-}

	print $ find_to_list 3 l1
	print $ find_from_list 3 l1
	print $ splitAt 4 l1
	print $ new_splitAt 3 l1

	print $ l1 !! 3 --double exclamation k accesses the element at the kth index of the list in LINEAR time
	print $ (!!) l1 3 --overloading the (!!) operator to use in the prefix form

	let l2 = ['a'..'i']
	print $ new_splitAt 3 l2

	print $ filter is_bignum l1

	let l3 = ['j','A','b','d','l','c','e']
	let l4 = [6,12,4,8,2,10]
	let l5 = [2,6,10,14,18]
	let l6 = [1,4,1,6,5,6,2,4,2]
	print $ insertion_sort l4
	print $ insertion_sort l3
	print $ merge l1 l5

	print $ find_to_list (div (length l4) 2) l4
	print $ find_from_list (div (length l4) 2) l4

	print $ merge_sort l6
	print $ quicksort l6
	print $ quicksort_compact l6