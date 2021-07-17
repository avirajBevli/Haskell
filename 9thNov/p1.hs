--arrays are used more in procedural programming
--we prefer not to use arrays in haskell, 
--we generally use lists in functional programming
--because these are more natural with lazy evaluation, recursion, mathematical definitions
import Data.Array

array1 = listArray (0,4) ['g','h','a','b','c'] 
--lower bound, upper bound of indices (included)
array2 = listArray ('a','d') [3,5,9,1]
--can use the characters as array indices as well
--(which is not allowed in other languages)

--array3 = listArray (1.0,2.4) [3,5,9,1] --not allowed

array3 = listArray ('a','d')[1..] 
--this works because of Haskell's lazy evaluation

array4 = listArray (0,3)['a','b'] 
--run time ERROR -> p1: (Array.!): undefined array element

main = do
	let l1 = [1..5]
	print $ l1
	print $ array1
	print $ array2
	print $ array3
	--print $ array4 --p1: (Array.!): undefined array element

	--to access the kth element of the list
	--Linear time operator
	print $ l1 !! 1 --gives empty array, because haskell implicitly assumes
	--that the first index is smaller than the second index

	--print $ l1 !! 5 --ERROR -> prelude -> index is too large
	--constant time operator
	print $ array1 ! 3

	print $ range(1,3)
	print $ range('f','n')
	--print $ range(1.0,2.0)--error 

	print $ index (25,75) 30 --finds the index of 30 in {25,..,75}
	--print $ index (25,75) 20 --out of range run time error

	print $ inRange (25,75) 30 --returns a boolean value
	print $ inRange (25,75) 20 --returns false

	print $ rangeSize (25,75)
	print $ bounds array1 --gives the index bounds with which the aray was constructed
	print $ bounds array2

	--list of the indices of the array
	print $ indices array1
	print $ indices array2--list of chars is a string

	print $ elems array1--print the elemenets of the array
	print $ assocs array1--index along with the elements
	
	print $ (//) array1 [(3,'d')]--the array can be updated using the index
	--but the change is not permanent change
	print $ array1

	
	 