--18MA20009
--Aviraj Singh Bevli
data Btree a = Empty | Node (Btree a) a (Btree a)

--inorder - Left, Root, Right
inorder :: Btree Int -> [Int]
inorder Empty = []
inorder (Node Empty x Empty) = [x]
inorder (Node lt x rt) = (inorder lt) ++ [x] ++ (inorder rt)

nodes_at_h :: Btree Int -> Int -> [Int]
nodes_at_h Empty _ = []
nodes_at_h (Node lt x rt) h
    | h == 1 = [x]
    | h > 1  = (nodes_at_h lt (h-1)) ++ (nodes_at_h rt (h-1))
    | otherwise = []

height :: Btree Int -> Int
height Empty = 0
height (Node lt x rt) = (max (height lt) (height rt)) + 1

nodes_till_h :: Btree Int -> Int -> [Int]
nodes_till_h Empty x = []
nodes_till_h (Node lt x rt) h = (nodes_at_h lt (h-1)) ++ (nodes_at_h rt (h-1)) ++ (nodes_till_h lt (h-1)) ++ (nodes_till_h rt (h-1))

main = do
	let t1 = Node Empty 5 Empty
	let t3 = Node (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) 6 (Node Empty 8 Empty) --BST
	print $ inorder t3
	print $ height t3
	print $ nodes_at_h t3 3
	--print $ nodes_till_h t3 (height t3)--not working