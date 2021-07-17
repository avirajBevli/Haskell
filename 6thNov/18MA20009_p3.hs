--Aviraj Singh Bevli
--18MA20009
{-
3) Write a quicksort program. Write two separate functions for filtering the elements smaller and greater than the pivot, 
and use them to obtain your quicksort function. [5 marks]
-}

--List of elements in the list which are smaller than or equal to a particular value (key)
smaller_elems :: Int -> [Int] -> [Int]
smaller_elems key [] = [] --return empty list for an empty list
--if x is smaller than or equal to key, add it to the list and search for other smaller elements, 
--else drop it from the list and search for other smaller elements
smaller_elems key (x:xs)
	| (x<=key) = x:(smaller_elems key xs)--we take equal to sign as well to handle arrays with repitition
	| (x>key) = smaller_elems key xs

--List of elements in the list which are bigger than a particular value(key)
bigger_elems :: Int -> [Int] -> [Int]
bigger_elems key [] = [] --return empty list for an empty list
--if x is bigger than the key, add it to the list and search for other bigger elements, 
--else drop it from the list and search for other bigger elements
bigger_elems key (x:xs)
	| (x>key) = x:(bigger_elems key xs)--we do not take equal to sign here to avoid a single entry being considered more than once in the sorted list
	| (x<=key) = bigger_elems key xs

quicksort :: [Int] -> [Int]
quicksort [] = []
--(List containing all the elements smaller than or equal to x in the list xs) ++ [x] ++ (List containing all the elements greater than x in the list xs)
quicksort (x:xs) = (quicksort (smaller_elems x xs)) ++ [x] ++ (quicksort (bigger_elems x xs))
--We use [x] here instead of x because ++ is the list concatenation operator and operates only on lists

main = do
	let l1 = [3,2,10,2,10,7,41,8,13]
	print $ l1
	print $ quicksort l1
	let l2 = [-2,2,2,1,2,-3,4,3,0,1]
	print $ l2
	print $ quicksort l2
