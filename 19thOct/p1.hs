--Haskell programming bitch

newlength :: [Integer] -> Integer
newlength [] = 0
newlength (x:xs) = 1 + newlength xs 
--x is the head of the list, xs is the tail of the list
--first element of the list followed by all the other elements of the list
	
{-
newlength (first_element:[the remaining list])
newlength (6:[8,10,12]) = 1 + newlength(8:[12,12]) = 1 + (1 + newlength(10:[12]))
=1+(1+(1+newlength(12:[]))) = 1+(1+(1+(1+newlength([]))) = 1+(1+(1+(1+0))) = 4
-}

newhead :: [Integer] -> Integer --user defined head function which shows an error for an empty list input 
newhead [] = error "List has no head!!"
newhead (x:xs) = x

newtail :: [Integer] -> [Integer]
newtail [] = []
newtail (x:xs) = xs

newreverse :: [Integer] -> [Integer]
newreverse [] = []
--Lesson : non-recursive kind of a definition is also allowed for lists
newreverse (x:xs) = newreverse (xs) ++ [x]

newlast :: [Integer] -> Integer
newlast [] = error "Empty list given as input!"
newlast (x:[]) = x
newlast (x:xs) = newlast(xs)
{-
newlast1 :: [Integer] -> Integer
newlast1 (x:xs) = newhead newreverse (x:xs) -}

newinit :: [Integer] -> [Integer]
newinit (x:[]) = []
newinit (x:xs) = [x] ++ newinit xs


{-
Writing functions in an infix manner is not allowed, 
because normally when we write on paper we do this way only

-}
access :: [Integer] -> Integer -> Integer
access [] idx = error "Index too large!"
access (x:xs) 0 = x
access (x:xs) idx = access xs (idx-1)

newsum :: [Integer] -> Integer
newsum [] = 0
newsum (x:xs) = x + newsum(xs)

newSplitAt :: Integer -> [Integer] -> [Integer] -> [Integer]
newSplitAt idx [] = error "Index too large!"
newSplitAt 0 (x:xs) = [x] xs
newSplitAt idx (x:xs) = newSplitAt (idx-1) xs 


main = do
	let l1 = [1,2,3]--list in kaskell
	print $ l1

	let l2 = [7..20]
	print $ l2

	let l3 = [6,8..12]--AP kind of a thing with common difference given by the first two numbers
	print $ l3

	print $ l3 ++ [49,50]--list concatenation
	print $ length l2 --length is inbuilt function
	print $ newlength l3 --newlength is our definition

	--this gives an error
	let l4 = [1]
	--print $ head l4
	--print $ newhead l4

	print $ tail l4
	print $ newtail l4

	print $ reverse l3
	print $ newreverse l3

	print $ last l3
	print $ newlast l3
	--print $ newlast1 l3

	print $ init l1
	print $ newinit l1

	print $ l3 !! 3 --double exclamation k accesses the element at the kth index of the list in LINEAR time
	print $ (!!) l3 3 --overloading the (!!) operator to use in the prefix form
	print $ access l3 2 --access the 2nd element

	print $ sum l3
	print $ newsum l3

	print $ splitAt 2 l3
	print $ newSplitAt 2 l3