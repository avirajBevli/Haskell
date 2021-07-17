--inorder representation
--a is an abstract data type...if we want a tree with int values only,
--then use data Btree Int = Empty | Node (Btree Int) Int (Btree Int)
data Btree a = Empty | Node (Btree a) a (Btree a)

flatten :: Btree Int -> [Int]
flatten Empty = []
flatten (Node lt x rt) = (flatten lt) ++ [x] ++ (flatten rt)

find_root :: Btree Int -> Int
find_root Empty = 0
find_root (Node lt x rt) = x

max :: Int -> Int -> Int -> Int
max a b c
	| (a>=b && a>=c) = a
	| (b>=a && b>=c) = b
	| (c>=b && c>=a) = c

min :: Int -> Int -> Int -> Int
min a b c
	| (a<=b && a<=c) = a
	| (b<=a && b<=c) = b
	| (c<=b && c<=a) = c

find_max :: Btree Int -> Int
find_max Empty = -1000
find_max (Node lt x rt) = max (find_max lt) x (find_max rt)

find_min :: Btree Int -> Int
find_min Empty = 10000
find_min (Node lt x rt) = min (find_min lt) x (find_min rt)

is_BST :: Btree Int -> Bool
is_BST Empty = True
is_BST (Node lt x rt) = (x > (find_max lt)) && (x < (find_min rt)) && (is_BST lt) && (is_BST rt)

main = do
	let l1 = [1..5]
	let t1 = Node Empty 5 Empty
	let t2 = Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty) 
	print $ l1
	print $ flatten t2
	print $ flatten t1
	print $ is_BST t1