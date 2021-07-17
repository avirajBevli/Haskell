data Nat = Zero | Succ Nat
data List a = Nil | Cons a (List a)
data Btree a = Empty | Node (Btree a) a (Btree a)

power :: Float -> Nat -> Float
power x Zero = 1.0
power x (Succ n) = x * (power x n)

plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ (n)) = Succ (plus m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = plus m (mult m n)

convtoint :: Nat -> Int
convtoint Zero = 0
convtoint (Succ n) = 1 + (convtoint n)

newhead :: List a -> a 
newhead Nil = error "List is empty"
newhead (Cons x (_)) = x

flatten :: Btree Int -> [Int] 
flatten Empty = []
flatten (Node lt x rt) = (flatten lt) ++ [x] ++ (flatten rt)

{-find_max_elem :: Btree Int -> Int
find_max_elem Empty = -10000
find_max_elem (Node Empty x Empty) = x
find_max_elem (Node lt x Empty) = max x (find_max_elem lt)
find_max_elem (Node Empty x rt) = max x (find_max_elem rt)
find_max_elem (Node lt x rt) = max x (max (find_max_elem lt) (find_max_elem rt))-}

find_max_elem :: Btree Int -> Int
find_max_elem Empty = -10000
find_max_elem (Node lt x rt) = max x (max (find_max_elem rt) (find_max_elem lt))

find_min_elem :: Btree Int -> Int
find_min_elem Empty = 10000
find_min_elem (Node lt x rt) = min x (min (find_min_elem rt) (find_min_elem lt))


find_min_elem_in_BST :: Btree Int -> Int
find_min_elem_in_BST Empty = 10000
find_min_elem_in_BST (Node Empty x Empty) = x
find_min_elem_in_BST (Node lt x rt) = find_min_elem lt

find_max_elem_in_BST :: Btree Int -> Int
find_max_elem_in_BST (Node Empty x Empty) = x
find_max_elem_in_BST Empty = -10000
find_max_elem_in_BST (Node lt x rt) = find_min_elem rt


isBST :: Btree Int -> Bool
isBST Empty = True
isBST (Node lt x rt) = ((find_min_elem rt)>x) && ((find_max_elem lt)<x) && (isBST lt) && (isBST rt)


insert :: Btree Int -> Int -> Btree Int
insert Empty key = (Node Empty key Empty)
insert (Node lt x rt) key
	| (key == x) = (Node lt x rt)--no insertion required in this case
	| (key <= x) = (Node (insert lt key) x rt)
	| otherwise = (Node lt x (insert rt key))

search :: Btree Int -> Int -> Bool
search Empty key = False
search (Node lt x rt) key
	| (key==x)  = True
	| (key < x) = (search lt key) 
	| (key > x) = (search rt key)

--inorder - Left, Root, Right
inorder :: Btree Int -> [Int]
inorder Empty = []
inorder (Node Empty x Empty) = [x]
inorder (Node lt x rt) = (inorder lt) ++ [x] ++ (inorder rt)

--preorder - Root, Left, Right
preorder :: Btree Int -> [Int]
preorder Empty = []
preorder (Node Empty x Empty) = [x]
preorder (Node lt x rt) = [x] ++ (preorder lt) ++ (preorder rt)

--postorder - Left, Right, Root
postorder :: Btree Int -> [Int]
postorder Empty = []
postorder (Node Empty x Empty) = [x]
postorder (Node lt x rt) = (postorder lt) ++ (postorder rt) ++ [x]


deleteRoot :: Btree Int -> Btree Int
deleteRoot (Node Empty x Empty) = Empty
deleteRoot (Node Empty x rt) = rt
deleteRoot (Node lt x Empty) = lt
deleteRoot (Node lt x rt) = (Node lt val rt2) --(delete t2 v2))
	where 
		val = find_min_elem_in_BST rt
		rt2 = delete rt (find_min_elem_in_BST rt)

delete :: Btree Int -> Int -> Btree Int
delete Empty key = error "The value to be deleted not found in BST"
delete (Node lt x rt) key
	| (key<x) = (Node (delete lt key) x rt)
	| (key>x) = (Node lt x (delete rt key))
	| (key==x) = deleteRoot (Node lt x rt)

main = do
	let l1 = [1,3..17]
	let n1 = (Succ (Succ Zero)) --like 2
	let n2 = (Succ(Succ (Succ Zero))) --like 3
	--print $ power 3.0 n1
	--print $ plus n1 n2 -> cant do because compier does not know how to print out data type
	--print $ convtoint (plus n1 n2)
	--print $ convtoint (mult n1 n2)
	--print $ newhead (Cons 1 ( Cons 4 (Cons 3 (Cons 2 (Nil))) ))

	let t1 = Node Empty 5 Empty
	let t3 = Node (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) 6 (Node Empty 8 Empty) --BST
	let t4 = Node (Node (Node Empty 9 Empty) 5 (Node Empty 4 Empty)) 11 (Node Empty 7 Empty) --not valid BST

	--print $ l1
	--print $ flatten t2
	--print $ flatten t3
	--print $ flatten t4
	--print $ isBST t2
	--print $ isBST t3
	--print $ isBST t4
	--print $ flatten (insert t3 7)

	--print $ flatten t3
	let t5 = insert t3 1
	--print $ flatten t5
	let t6 = insert t5 2
	print $ flatten t6
	let t7 = delete t6 3
	print $ flatten t7

	--print $ search t4 2
	--print $ search t6 2

	--print $ flatten t6
	--print $ inorder t6
	--print $ preorder t6
	--print $ postorder t6