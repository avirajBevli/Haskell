data Btree a = Empty | Node (Btree a) a (Btree a)
{-
is_bst :: Btree Int -> Bool
is_bst (Node Empty x Empty) = True
is_bst (Node Empty x (Node rlt rv rrt)) = (is_bst (Node rlt rv rrt)) && (x <= rv)
is_bst (Node (Node llt lv lrt) x Empty) = (is_bst (Node llt lv lrt)) && (lv <= x)
is_bst (Node (Node llt lv lrt) x (Node rlt rv rrt)) = (is_bst (Node llt lv lrt)) && (is_bst (Node rlt rv rrt)) && (lv <= x) && (x <= rv)
-}

flatten :: Btree Int -> [Int]
flatten Empty = []
flatten (Node lt x rt) = (flatten lt) ++ [x] ++ (flatten rt)

insert:: Btree Int -> Int -> Btree Int
insert Empty a = Node Empty a Empty
insert (Node lt x rt) a
	| (x > a) = (Node (insert lt a) x rt)
	| otherwise = (Node lt x (insert rt a))

main = do
	let l1 = [1..5]
	let t1 = Node Empty 5 Empty
	let t2 = Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty) 
	print $ l1
	--insert t1 3
	print $ flatten t1