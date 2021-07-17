power :: Integer -> Integer -> Integer
power x 0 = 1
power x n = x*(power x (n-1))

--Only one data type allowed to be used in the template
newreverse :: [a] -> [a]
newreverse [] = []
newreverse (x:xs) = newreverse (xs) ++ [x]

newhead :: [a] -> a --user defined head function which shows an error for an empty list input 
newhead [] = error "List has no head!!"
newhead (x:xs) = x

access :: [a] -> Integer -> a
access [] idx = error "Index too large!"
access (x:xs) 0 = x
access (x:xs) idx = access xs (idx-1)

newSplitAt_helper :: [Integer] -> [Integer] -> Integer -> ([Integer], [Integer])
newSplitAt_helper l1 [] idx = error "Split - Index out of bounds !!"
newSplitAt_helper l1 (x : xs) 0 = (l1 ++ [x], xs)
newSplitAt_helper l1 (x : xs) idx = newSplitAt_helper (l1 ++ [x]) xs (idx - 1) 

newSplitAt :: Integer -> [Integer] -> ([Integer], [Integer])
newSplitAt idx l1 = newSplitAt_helper [] l1 idx

main = do
	let l1 = [1,2,3]
	print $ l1
	print $ x+y
	print $ (+) x y
	print $ mod x y
	print $ a ^ b
	print $ power (div x y) b
	print $ div 8 3
	--print $ div c 3 --gives error 
	--" error: Variable not in scope: c :: Integer"

	--Demonstration of lazy evaluation, Call by need by Haskell
	print $ power (div 3 0) 0 --since Haskell uses call by need, 
	--the computation inside the bracket will not occur since 
	--the case power x 0 matches and we return 1 simply
	--print $ power (div 3 0) 1 --now it will throw an error of divide by zero

	--print $ [1..]--infinite list will be printed
	print $ head [3..]--another example of call by need

	print $ reverse [1..10]
	print $ newreverse [1..10]
	print $ reverse [1.0..10.0]
	print $ newreverse [1.0..10.0]
	print $ reverse ['a'..'e']
	print $ newreverse ['a'..'e']

	print $ newhead ['a'..'e']
	print $ newhead [1.0..10.0]

	print $ access [1.0..10.0] 3

	print $ newSplitAt 3 [1..10]

	--print $ div 7 0--this gives an error as opposed to the lazy
	--evaluation example discussed above

	where
		x=17
		y=5
		a=4
		b=3