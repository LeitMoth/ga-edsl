{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GA () where
import Data.Function (on)

data FuncThing = Exp
  deriving (Show)

data M2 a where
  Scal :: a -> M2 a
  Basis1 :: M2 a
  Basis2 :: M2 a
  Basis3 :: M2 a
  Mul :: M2 a -> M2 a -> M2 a
  Sum :: M2 a -> M2 a -> M2 a
  Func :: FuncThing -> M2 a -> M2 a
  deriving Show

e1 = Basis1
e2 = Basis2
e3 = Basis3

mexp :: Floating a => M2 a -> M2 a
mexp a = Func Exp a

instance Functor M2 where
  fmap f (Scal a) = Scal (f a)
  fmap f (Mul a b) = Mul (fmap f a) (fmap f b)
  fmap f (Sum a b) = Sum (fmap f a) (fmap f b)
  fmap _ Basis1 = Basis1
  fmap _ Basis2 = Basis2
  fmap _ Basis3 = Basis3

instance Num a => Num (M2 a) where
  (+) = Sum
  (*) = Mul
  abs = error "not implemented"
  signum = error "not implemented"
  fromInteger = Scal . fromInteger
  negate = fmap negate

simplify :: Num a => M2 a -> M2 a
simplify = \case
  Scal a -> Scal a
  Basis1 -> Basis1
  Basis2 -> Basis2
  Basis3 -> Basis3
  Mul a (Mul b c) -> fixup $ Mul (Mul a b) c
  Mul Basis2 Basis1 -> Mul (-1) (Mul Basis1 Basis2)
  Mul Basis1 Basis3 -> Mul (-1) (Mul Basis3 Basis1)
  Mul Basis3 Basis2 -> Mul (-1) (Mul Basis2 Basis3)
  Mul Basis1 Basis1 -> Scal 1
  Mul Basis2 Basis2 -> Scal 1
  Mul Basis3 Basis3 -> Scal 1
  Mul a b -> fixup (Mul (simplify a) (simplify b))
  Sum a b -> fixup (Sum (simplify a) (simplify b))

fixup :: Num a => M2 a -> M2 a
fixup = orderBasis . pullScalar

pullScalar :: Num a => M2 a -> M2 a
pullScalar (Mul (Scal a) (Scal b)) = Scal (a*b)
pullScalar (Mul a (Scal b)) = pullScalar (Mul (Scal b) a)
pullScalar (Mul (Mul (Scal a) b) c) = Mul (Scal a) (Mul b c)
pullScalar (Mul c (Mul (Scal a) b)) = Mul (Scal a) (Mul b c)
pullScalar a = a

orderBasis :: Num a => M2 a -> M2 a
orderBasis = \case
  Mul Basis2 Basis1 -> Mul (-1) (Mul Basis1 Basis2)
  Mul Basis1 Basis3 -> Mul (-1) (Mul Basis3 Basis1)
  Mul Basis3 Basis2 -> Mul (-1) (Mul Basis2 Basis3)
  Mul (Mul Basis2 Basis3) Basis1 -> Mul (Mul Basis1 Basis2) Basis3
  Mul a b -> Mul (orderBasis a) (orderBasis b)
  a -> a

data M3 a b where
  Base :: b -> M3 a b
  Prod :: a -> [M3 a b] -> M3 a b
  Sum2 :: [M3 a b] -> M3 a b
  deriving (Show)

funmap :: (M3 a b -> M3 a b) -> M3 a b -> M3 a b
funmap f (Base b) = Base b
funmap f (Prod s ms) = Prod s (map f ms)
funmap f (Sum2 ms) = Sum2 (map f ms)

data Basis = X | Y | Z
  deriving (Show)

broil :: Num a => M2 a -> M3 a Basis
broil = \case
  Scal a -> Prod a []
  Basis1 -> Base X
  Basis2 -> Base Y
  Basis3 -> Base Z
  Mul a b -> Prod 1 [broil a, broil b]
  Sum a b -> Sum2 [broil a, broil b]


-- distribute

bake :: Num a => M3 a b -> M3 a b
bake = \case
  Prod a ((Prod b bs) : cs) -> bake $ Prod (a*b) (map bake bs ++ map bake cs)
  Sum2 ((Sum2 bs) : cs) -> bake $ Sum2 (bs ++ cs)
  a -> a

-- normalize :: Num a => M3 a b -> M3 a b
-- normalize = pullProd

pullProd :: Num a => M3 a b -> M3 a b
pullProd = \case
  Prod s ms -> foldr f (Prod s []) ms
    where
      f a (Prod s1 ms1) = case pullProd a of
        Prod s2 ms2 -> Prod (s1*s2) (ms2 ++ ms1)
        m -> Prod s1 (m : ms1)
      f _ _ = error "BUG, should always be a Prod"
  Sum2 ms -> funmap pullProd (pullSum (Sum2 ms))
  a -> a

pullSum :: Num a => M3 a b -> M3 a b
pullSum = \case
  Sum2 ms -> foldr f (Sum2 []) ms
    where
      f a (Sum2 ms1) = case a of
        Sum2 ms2 -> Sum2 (ms2 ++ ms1)
        m -> Sum2 (m : ms1)
      f _ _ = error "BUG, should always be a Sum2"
  a -> a

distribute :: Num a => M3 a b -> M3 a b
distribute = \case
  Base b -> Base b
  Sum2 ms -> Sum2 ms
  Prod s ms -> foldr f (Prod 1 [Sum2 []]) ms
    where
      f a (Prod _ [Sum2 ms1]) = case a of
        Sum2 ms2 -> Sum2 (ms2 ++ ms1)
        m -> Sum2 (m : ms1)
      f _ _ = error "BUG, should always be a Sum2"

distributeFromRight :: Num a => a -> [M3 a b] -> M3 a b -> M3 a b
distributeFromRight ambient ms p = Sum2 $ map (\x -> Prod ambient [x,p]) ms

distributeFromLeft :: Num a => a -> M3 a b -> [M3 a b] -> M3 a b
distributeFromLeft ambient p ms = Sum2 $ map (\x -> Prod ambient [p,x]) ms

distribute2 :: Num a => a -> [M3 a b] -> [M3 a b]
distribute2 ambient = \case
  ((Sum2 ms):p2:rest) -> distribute2 ambient (distributeFromRight ambient ms p2 : rest)
  (p1:(Sum2 ms):rest) -> distribute2 ambient (distributeFromLeft ambient p1 ms : rest)
  (m1 : m2 :rest) -> distribute2 ambient $ flatten [m1, m2] : rest
  [Sum2 ms] -> ms
  [m] -> [m]
  [] -> []

flatten :: Num a => [M3 a b] -> M3 a b
flatten = \case
  [] -> Prod 0 []
  (Prod s1 ms1 : Prod s2 ms2 : rest) -> (flatten (Prod (s1*s2) (ms1 ++ ms2) : rest))
  [Prod s1 ms1] -> Prod s1 ms1
  (p : rest) -> flatten (Prod 1 [p] : rest)


-- ghci> map (foldr (++) []) $ sequence $ [a,b]

data M4 a b where
  M4 :: [[Atom a b]] -> M4 a b
  deriving (Show)

data Atom a b where
  Base2 :: b -> Atom a b
  Scalar2 :: a -> Atom a b
  Func2 :: FuncThing -> M4 a b -> Atom a b
  deriving (Show)

m4mult :: M4 a b -> M4 a b -> M4 a b
m4mult (M4 a) (M4 b) = M4 $ map concat (sequence [a,b])

m4add :: M4 a b -> M4 a b -> M4 a b
m4add (M4 a) (M4 b) = M4 $ a ++ b

toM4 :: M2 a -> M4 a Basis
toM4 = \case
  Scal a -> M4 [[Scalar2 a]]
  Basis1 -> M4 [[Base2 X]]
  Basis2 -> M4 [[Base2 Y]]
  Basis3 -> M4 [[Base2 Z]]
  Mul a b -> (m4mult `on` toM4) a b
  Sum a b -> (m4add `on` toM4) a b
  Func f a -> M4 [[Func2 f (toM4 a)]]

normalize :: Num a => M3 a b -> M3 a b
normalize (Prod s ms) = Sum2 (distribute2 s ms)
normalize (Sum2 ms) = pullSum $ Sum2 (map normalize ms)
normalize (Base b) = Sum2 [Base b]


pretty :: (Show a, Num a, Eq a, Show b) => M3 a b -> String
pretty = \case
  Base b -> show b
  Prod 1 ms -> foldr (\x y -> pretty x ++ y) "" ms
  Prod s ms -> show s ++ "(" ++ foldr (\x y -> pretty x ++ y) "" ms ++ ")"
  Sum2 [] -> ""
  Sum2 ms -> foldr1 (\x y -> x ++ "+" ++ y) (map pretty ms)
