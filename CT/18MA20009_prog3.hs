--18MA20009
--Aviraj Singh Bevli
{-3) Extend the natural numbers "Nat" datatype defined in class to integers, 
by introducing user defined signs. Define addition, multiplication and convert to int, 
for your new datatype, so that the functions may now accommodate negative integers too. 
Define a new subtraction function as well.  -}
data Nat = Zero | Succ Nat

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

minus :: Nat -> Nat -> Nat
minus m Zero = m
minus m (Succ (n)) = Succ (minus m n)

main = do
	let n1 = (Succ (Succ Zero)) --like 2
	let n2 = (Succ(Succ (Succ Zero))) --like 3
	print $ convtoint (plus n1 n2)