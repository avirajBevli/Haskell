--18MA20009
--Aviraj Singh Bevli
{-
Extend the natural numbers "Nat" datatype defined in class to integers, 
by introducing user defined signs. Define addition, multiplication and convert to int, 
for your new datatype, so that the functions may now accommodate negative integers too. 
Define a new subtraction function as well.  [10 marks]
-}

data Nat = Zero | Succ Nat | Prev Nat-- non negative integers

power :: Float -> Nat -> Float
power x Zero = 1.0
power x (Succ n) = x * power x n 
power x (Prev n) = (power x n)/x 

plus :: Nat -> Nat -> Nat 
plus m Zero = m 
plus m (Succ (n)) = Succ (plus m n)
plus m (Prev (n)) = Prev (plus m n)

minus :: Nat -> Nat -> Nat 
minus m Zero = m 
minus Zero m = Neg m
minus m (Succ (n)) = Succ (minus m n)
minus m (Prev (n)) = Prev (minus m n)

mult :: Nat -> Nat -> Nat 
mult m Zero = Zero
mult m (Succ n) = plus (mult m n) m
mult m (Prev n) = minus (mult m n) m  -- m ( n - 1) = m*n - m

convtoint :: Nat -> Int -- convert to int
convtoint Zero = 0  -- converts Zero (which is user defined) to actual 0
convtoint (Succ n) = 1 + (convtoint n) 
convtoint (Prev n) = (convtoint n) - 1

main = do
	let n1 = (Succ (Succ Zero) )--2
	let n2 = (Succ (Succ (Succ Zero) ))--3
	let n3 = (Prev (Prev (Prev Zero) ))-- -3
	print $ power 3.0 (Succ (Succ Zero) )
	print $ power 3.0 (Succ (Prev (Succ(Zero)) ) )
	print $ convtoint (plus n1 n2)
	print $ convtoint (mult n1 n3)
	print $ convtoint (minus n1 n3)