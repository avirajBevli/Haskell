--18MA20009
--Aviraj Singh Bevli

--de onlb conisder sqrs_sumbers such that a<=b<=c=<=d
sqrs_sum :: Int -> [(Int, Int, Int, Int)]
sqrs_sum n = [(a, b, c, d) | a <- [1..n], b <- [a..(n-a*a)], c <- [b..(n-a*a-b*b)], d <- [c..(n-a*a-b*b-c*c)], (a*a + b*b + c*c + d*d)==n]

pretty :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int)
pretty (x:xs) = x

main = do
	print $ pretty (sqrs_sum 23)
	print $ pretty (sqrs_sum 67)
	print $ pretty (sqrs_sum 98)