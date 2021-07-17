--inorder representation
--a is an abstract data type...if we want a tree with int values only,
--then use data Btree Int = Empty | Node (Btree Int) Int (Btree Int)
data Btree a = Empty | Node (Btree a) a (Btree a)

flatten :: Btree a -> [a]
flatten Empty = []
flatten (Node lt x rt) = (flatten lt) ++ [x] ++ (flatten rt)

main = do
	let l1 = [1..5]
	let t1 = Node Empty 5 Empty
	let t2 = Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty) 
	print $ l1
	print $ flatten t2
	print $ flatten t1