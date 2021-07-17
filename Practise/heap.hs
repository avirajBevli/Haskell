data Heap a = HNil | HNode a Int (Heap a) (Heap a) --HNode has 4 entries, entry, size_heap(Intger datatype), left_heap, right_heap

root :: Ord a => Heap a -> a
root (HNode x sz hl hr) = x

size :: Ord a => Heap a -> Int
size HNil = 0
size (HNode x sz hl hr) = sz

is_heap :: Ord a => Heap a -> Bool
is_heap HNil = True
is_heap (HNode x sz HNil HNil) = True
is_heap (HNode x sz HNil hr) = (is_heap hr) && (x>=(root hr))
is_heap (HNode x sz hl HNil) = (is_heap hl) && (x>=(root hl))
is_heap (HNode x sz hl hr) = (is_heap hl) && (is_heap hr) && (x>=(root hl)) && (x>=(root hr))

mkbtree :: Ord a => [a] -> Heap a
--access first element of the pair, ie access the Heap
mkbtree l = fst ( mkbtreeaux l (length l) )

mkbtreeaux :: Ord a => [a] -> Int -> (Heap a, [a])
mkbtreeaux [] n = (HNil, [])
mkbtreeaux l 0  = (HNil, l)
mkbtreeaux l n  = (HNode x (1+szl+szr) hl hr, l2)
    where
        m               =   n `div` 2 
        (hl, x:rest)    =   mkbtreeaux l m
        (hr, l2)        =   mkbtreeaux rest (n-(m+1))
        szl             =   size hl
        szr             =   size hr

main = do
	let h1 = HNode 1 1 HNil HNil
	print $ root h1
	print $ size h1
	print $ is_heap h1
	let l1 = [1,41,21,22,6,9,2]
	let h2 = (mkbtree l1)
	print $ size h1