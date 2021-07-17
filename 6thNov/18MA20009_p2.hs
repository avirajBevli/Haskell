--Aviraj Singh Bevli
--18MA20009
{-
2) Given a list of lists of integers, write a program to collapse its inner brackets and express it as a single list of integers. [5 marks]
-}

collapse :: [[Int]] -> [Int]
collapse [] = [] --if the list of lists is empty, then the collapsed list will be empty
collapse (x:xs) = x ++ (collapse xs) --concatenate the list x with the collapsed list formed by applying the collapse function of the list of lists xs
--We use x and not [x] here because x is itself a list and the list concatenation operator(++) only operates on lists

main = do
	let l1 = [[4,2,0],[1,3],[9,7,-1],[0,0,10],[-1,9,2]]
	print $ l1
	print $ collapse l1
