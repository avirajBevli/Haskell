-- Name : Altaf Ahmad, Roll no: 18MA20005
data Nat = Zero | Succ Nat 
    deriving Show

plus :: Nat -> Nat -> Nat
plus x Zero = x
plus Zero x = x
plus n (Succ (m)) = Succ (plus n m) 

{-
plus (Succ (Succ Zero) )  (Succ (Succ (Succ Zero) ))

= Succ ( plus  Succ (Succ Zero)    (Succ (Succ Zero) )     )

= Succ ( Succ ( plus  Succ (Succ Zero)   (Succ Zero)  ))


= Succ ( Succ ( Succ  (plus    Succ (Succ Zero)     Zero    ) ))

= Succ ( Succ ( Succ  (     Succ (Succ Zero)        ))


-}

minus :: Nat -> Nat -> Nat
minus x Zero = x
minus Zero x = x
minus (Succ (n)) (Succ (m)) = minus n m 

converttoint :: Nat -> Int 
converttoint Zero = 0
converttoint (Succ (x)) = 1 + (converttoint x)

multiplynumb :: Nat-> Nat -> Nat
multiplynumb Zero _ = Zero
multiplynumb _ Zero = Zero
multiplynumb x (Succ (y)) = plus x (multiplynumb x y)

{-
convtoint :: Nat -> Int -- convert to int
convtoint Zero = 0  -- converts Zero (which is user defined) to actual 0
convtoint (Succ n) = 1 + (convtoint n) 
-}


data Zint = Zint Nat Int
    deriving Show

additionzint :: Zint -> Zint -> Zint
additionzint (Zint Zero _) x = x
additionzint x (Zint Zero _) = x
additionzint (Zint n sgn) (Zint m sgm)
    | sgn == sgm = (Zint (plus n m) sgn)
    | (converttoint n) > (converttoint m) = (Zint (minus n m) (sgn))
    | otherwise = (Zint (minus m n) ((-1)*sgn))


multiplyztoint :: Zint -> Zint -> Zint
multiplyztoint (Zint Zero _) _ = Zint Zero 1
multiplyztoint _ (Zint Zero _) = Zint Zero 1
multiplyztoint (Zint n sgn) (Zint m sgm) = (Zint (multiplynumb n m) (sgn*sgm))

subtractztoint :: Zint -> Zint -> Zint
subtractztoint (Zint Zero _) x = x
subtractztoint x (Zint Zero _) = x
subtractztoint (Zint n sgn) (Zint m sgm) = additionzint (Zint n sgn) (Zint m ((-1)*sgm))

convertztoint :: Zint -> Int
convertztoint (Zint Zero _) = 0
convertztoint (Zint x sgn) = (sgn)*(converttoint x)

data Rational = Rational Nat Zint
    deriving Show

additionrational :: Rational -> Rational -> Rational


main = do
    let x = plus (Succ Zero) (Succ (Succ Zero) )
    let ab = Zint x 1
    let bc = Zint x (-1)
    print $ convertztoint ab
    print $ convertztoint bc 