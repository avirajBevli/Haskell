data Nat = Zero | Succ Nat --non negative integers

power :: Float -> Nat -> Float --works without this(the type signature) as well
power x Zero = 1.0
power x (Succ n) = x * power x n -- x raise to the power n+1 is x*x to the power n
{-
power 3.0 (Succ (Succ Zero)) 
=3.0 * power 3.0 (Succ Zero)
=3.0 * 3.0 * power 3.0 Zero 
=3.0 * 3.0 * 1.0 
=9.0
-}

plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ (n)) = Succ (plus m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = plus m (mult m n) --m(n+1) = m + mn 

convtoint :: Nat -> Int  --convret to int
convtoint Zero = 0
convtoint (Succ n) = 1 + (convtoint n)

data List a = Nil | Cons a (List a)
--our own list, recursive definition of the data type
--list can be nil, or it can be 

newhead :: List a -> a
newhead Nil = error "List is empty"
newhead (Cons x (_)) = x --(_) is standard notation -> means anything

main = do
	let l1 = [1..5]
	let n1 = (Succ (Succ Zero))
	let n2 = (Succ (Succ (Succ Zero)))
	print $ l1
	print $ newhead ( Const 4 (Cons 3 (Cons 2 (Nil))) )
	print $ power 3 (Succ (Succ Zero))
	--print $ n1
	print $ convtoint (plus n1 n2)
	print $ convtoint (mult n1 n2)