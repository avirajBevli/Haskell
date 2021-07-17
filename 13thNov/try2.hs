data Nat = Zero | Succ Nat -- non Negativeative integers(Natural numbers)
data Int_z = Positive Nat | Negative Nat --this is like a boolean sign signifying whether the integer is positive or negative

-------------------------------------NATURAL NUMBER OPERATIONS----------------------
power :: Float -> Nat -> Float
power x Zero = 1.0
power x (Succ n) = x * power x n 

plus :: Nat -> Nat -> Nat 
plus m Zero = m 
plus m (Succ (n)) = Succ (plus m n)

minus :: Nat->Nat->Nat
minus m Zero = m
minus (Succ m) (Succ n) = minus m n

mult :: Nat -> Nat -> Nat 
mult m Zero = Zero
mult m (Succ n) = plus m (mult m n)  -- m ( n + 1) = m + m*n

convtoint :: Nat -> Int -- convert to Int datatype
convtoint Zero = 0 
convtoint (Succ n) = 1 + (convtoint n)
------------------------------------------------------------------------------------

--Extension of Nat to Int
-------------------------------------INTEGERS OPERATIONS----------------------
plusint :: Int_z -> Int_z -> Int_z
plusint (Positive m) (Positive n) = Positive(plus m n)-- m+n = m+n, for m,n positive
plusint (Negative m) (Negative n) = Negative(plus m n)-- m+n = m+n, for m,n negative

plusint (Negative m) (Positive n)
	| (convtoint(n)>=convtoint(m)) = Positive(minus n m)-- n-m = n-m
	| otherwise = Negative(minus m n) -- -(n-m) = m-n

plusint (Positive m) (Negative n)
	| (convtoint(m)>=convtoint(n)) = Positive(minus m n) -- m-n = m-n
	| otherwise = Negative(minus n m)  -- -(m-n) = n-m


minusint :: Int_z->Int_z->Int_z
minusint (Positive m) (Positive n) = (plusint (Positive m) (Negative n))
minusint (Negative m) (Negative n) = (plusint (Negative m) (Positive n))
--minusint m (Positive n) = (plusint m (Negative n))
minusint (Positive m) (Negative n) = (plusint (Positive m) (Positive n))
minusint (Negative m) (Negative n) = (plusint (Negative m) (Positive n))
--minusint m (Negative n) = (plusint m (Positive n))

multint :: Int_z->Int_z->Int_z
multint (Positive m) (Positive n) = Positive(mult m n)
multint (Positive m) (Negative n) = Negative(mult m n)
multint (Negative m) (Positive n) = Negative(mult m n)
multint (Negative m) (Negative n) = Positive(mult m n)

convertint :: Int_z -> Int -- convert to Int datatype
convertint (Negative n) = (-1)*(convtoint n)
convertint (Positive n) = convtoint n
-------------------------------------------------------------------------------------

main = do
 let n1 = Positive(Succ (Succ Zero) )--(2)
 let n2 = Negative(Succ (Succ (Succ Zero) ))--(-3)
 let n3 = Negative(Succ (Succ (Succ (Succ Zero) )))--(-4)
 print $ convertint (plusint n1 n2)--(n1+n2)
 print $ convertint (minusint n1 n2)--(n1-n2)
 print $ convertint (minusint (multint n1 n2) n3) --(n1n2 - n3 = -6-(-4) = -2)