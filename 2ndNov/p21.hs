newmap :: (a -> b) -> [a] -> [b]
newmap f [] = []
newmap f (x:xs) = (f x):(newmap f xs)

is_even :: Int -> Bool
is_even x = mod x 2 == 0 --x is even or not?

newfilter :: (a -> Bool) -> [a] -> [a]
newfilter p [] = []
newfilter p (x:xs)
	| p x = x : (newfilter p xs)
	| otherwise = newfilter p xs

my_filter :: [Int] -> [Int]
my_filter [] = []
my_filter (x : xs)
    | mod x 2 == 0 = x : (my_filter xs)
    | otherwise = my_filter xs

{-newfoldr :: 
newfoldr [] = []
newfoldr (x : xs)-}

is_alot :: Float -> Bool
is_alot x = x > 5.5

--This is not correct
is_upper :: Char -> Bool
is_upper x = x > 'A'

main = do
	let l1 = [6,8..20]
	let l2 = []
	let l3 = [3,2,10,7,41,8,13]
	let l4 = [7.2,3.0,4.7,1.24,5.5,5.6]
	let l5 = ['a','b','C','A','z','/','.','D','d']

	print $ l1
	print $ map (+3) l1
	print $ map (+3) []
	print $ newmap (+3) l1
	print $ newmap (*3) l1
	print $ filter is_even l3
	print $ newfilter is_even l3
	print $ my_filter l3
	print $ foldr (+) 0 l3 --sum of all elements of l3
	--the function + starts acting from the right most element of the list
	print $ foldl (+) 0 l3
	print $ newfilter is_alot l4
	print $ newfilter is_upper l5
