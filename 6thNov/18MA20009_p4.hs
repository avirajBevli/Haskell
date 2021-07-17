--Aviraj Singh Bevli
--18MA20009
{-
4) Modify your quicksort function using list comprehension notation as much as you can. [5 marks]
-}

quicksort_compact :: [Int] -> [Int]  
quicksort_compact [] = []  --if the list is empty, return an empty list
--Take x as pivot element and partition the list xs about x
--Since we are comparing x with elements of the list xs, we can easily handle arrays with repeated entries as well 
quicksort_compact (x:xs) =   
    let smaller_than = quicksort_compact [y | y <- xs, y <= x]  --apply quicksort_compact on the list of elements in xs smaller than or equal to the pivot element(x)
        bigger_than = quicksort_compact [y | y <- xs, y > x]    --apply quicksort_compact on the list of elements in xs greater than the pivot element(x)
    in smaller_than ++ [x] ++ bigger_than  
    --smaller_than is a sorted list containing of elements in xs smaller than or equal to x
    --bigger_than is a sorted list containing of elements in xs smaller than x
    --Hence, we use list concatenation operator (++) to concatenate the lists smaller_than, [x], bigger_than to obtain the list of sorted entries
	
	--Note: We use [x] here instead of x because ++ is the list concatenation operator and operates only on lists

main = do
	let l1 = [3,2,10,2,10,7,41,8,13]
	print $ l1
	print $ quicksort_compact l1

	let l2 = [-2,2,2,1,2,-3,4,3,0,1]
	print $ l2
	print $ quicksort_compact l2
