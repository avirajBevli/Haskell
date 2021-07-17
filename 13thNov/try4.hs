--18MA20009
--Aviraj Singh Bevli
data Nat = Zero | Succ Nat -- non Negativeative integers(Natural numbers)
data Int_z = Positive Nat | Negative Nat --this is like a boolean sign signifying whether the integer is positive or negative
data Rat = Rational Int_z Int_z

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


--Extension of Int_z to Rational_nos
-------------------------------------RATIONAL NUMBER OPERATIONS----------------------
makeRat :: Int_z -> Int_z -> Rat 
makeRat (Positive a) (Positive b) = (Rational (Positive a) (Positive b))
makeRat (Positive a) (Negative b) = (Rational (Negative a) (Positive b))
makeRat (Negative a) (Positive b) = (Rational (Positive a) (Positive b))
makeRat (Negative a) (Negative b) = (Rational (Positive a) (Positive b))

numer :: Rat -> Int_z
numer (Rational n d) = n

denom :: Rat -> Int_z
denom (Rational n d) = d

showRat :: Rat -> String
showRat r = show (convertint (numer r)) ++ "/" ++ show (convertint (denom r))

plusRat :: Rat -> Rat -> Rat
plusRat (Rational n1 d1) (Rational n2 d2) = ( Rational ( plusint (multint n1 d2) (multint n2 d1) ) (multint d1 d2) )

minusRat :: Rat -> Rat -> Rat
minusRat (Rational n1 d1) (Rational n2 d2) = ( Rational ( minusint (multint n1 d2) (multint n2 d1) ) (multint d1 d2) )

multRat :: Rat -> Rat -> Rat
multRat (Rational n1 d1) (Rational n2 d2) =  ( Rational (multint n1 n2) (multint d1 d2) )

divRat :: Rat -> Rat -> Rat
divRat (Rational (Positive n1) d1) (Rational (Positive n2) d2) = ( Rational (multint (Positive n1) d2) (multint (Positive n2) d1) )
divRat (Rational (Positive n1) d1) (Rational (Negative n2) d2) = ( Rational (multint (Negative n1) d2) (multint (Positive n2) d1) )
divRat (Rational (Negative n1) d1) (Rational (Positive n2) d2) = ( Rational (multint (Negative n1) d2) (multint (Positive n2) d1) )
divRat (Rational (Negative n1) d1) (Rational (Negative n2) d2) = ( Rational (multint (Positive n1) d2) (multint (Positive n2) d1) )
-------------------------------------------------------------------------------------

main = do
 print $ show "Integer number operations ->"
 let n1 = Positive(Succ (Succ Zero) )--(2)
 let n2 = Negative(Succ (Succ (Succ Zero) ))--(-3)
 let n3 = Negative(Succ (Succ (Succ (Succ Zero) )))--(-4)
 print $ show "Sum of The 2 Integer numbers ->"
 print $ convertint (plusint n1 n2)--(n1+n2)
 print $ show "Difference of The 2 Rational numbers ->"
 print $ convertint (minusint n1 n2)--(n1-n2)
 print $ show "Difference of The Product of 2 Rational numbers ->"
 print $ convertint (minusint (multint n1 n2) n3) --(n1n2 - n3 = -6-(-4) = -2)

 print $ show "Rational number operations ->"
 let n4 = Positive(Succ (Succ (Succ (Succ (Succ Zero) ))))--(5)
 let n5 = Negative(Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero) ))))))--(-7)
 let r1 = (makeRat n1 n2)
 let r2 = (makeRat n4 n5)
 print $ show "The 2 Rational numbers ->"
 print $ showRat r1
 print $ showRat r2
 print $ show "Sum of The 2 Rational numbers ->"
 print $ showRat (plusRat r2 r1)
 print $ show "Difference of The 2 Rational numbers ->"
 print $ showRat (minusRat r2 r1)
 print $ show "Product of The 2 Rational numbers ->"
 print $ showRat (multRat r2 r1)
 print $ show "Division of The 2 Rational numbers ->"
 print $ showRat (divRat r2 r1)