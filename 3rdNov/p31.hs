is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n
    | (length [x | x <- [2..(n - 1)], mod n x == 0] == 0) = True
    | otherwise = False

sieve :: [Int] -> [Int]
sieve []=[]
sieve (x:xs) = x:(sieve [y | y<-xs, mod y x > 0])

main = do
	let l1 = [6,8..20]
	let l2 = []
	let l3 = [3,2,10,7,41,8,13]
	let l4 = [7.2,3.0,4.7,1.24,5.5,5.6]
	let l5 = ['a','b','C','A','z','/','.','D','d']

	--print $ [x |x <- l3, mod x 2 == 0]--like the set builder notation
	--print $ [x*x |x <- l3, mod x 2 == 0]

	--squares of all even numbers from 1 to 20
	--print $ [x*x |x <- [1..20], mod x 2 == 0]
	--print $ [(x,y) |x <- [1..5], y <- [1..5]]

	--print $ [(x,y,z) |x <- [1..2], y <- [1..2], z <- [1..2]]

	--All pythogorean triplets from 1 to 100
	--print $ [(x,y,z) |x <- [1..100], y <- [1..100], z <- [1..100], x*x+y*y==z*z, x<y]
	--print $ [x | x<-[1..100], mod 100 x == 0]--divisors of 100

	print $ [x | x<-[1..100], mod x 20 == 0 || mod x 30 == 0]--can put or conditions also with ||
	--can put and conditions with && as well(along with using ,)

	print $ is_prime 3
	print $ [x | x<-[1..100], is_prime x]
	print $ sieve [2..n]
	print $ sieve2 [2..n]
	where
		n=100
		sieve2 []=[]
		sieve2 (x:xs) = x:(sieve [y | y<-xs, mod y x > 0])
