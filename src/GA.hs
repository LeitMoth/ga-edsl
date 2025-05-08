{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GA (someFunc) where

-- type Scalar = Double

data M2 a where
  Scal :: a -> M2 a
  Basis1 :: M2 a
  Basis2 :: M2 a
  Basis3 :: M2 a
  Mul :: M2 a -> M2 a -> M2 a
  Sum :: M2 a -> M2 a -> M2 a
  deriving Show

e1 = Basis1
e2 = Basis2
e3 = Basis3

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

normalize :: Num a => M3 a b -> M3 a b
normalize = pullProd

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

distributeFromRight :: Num a => [M3 a b] -> M3 a b -> M3 a b
distributeFromRight ms p = Sum2 $ map (\x -> Prod 1 [x,p]) ms

distributeFromLeft :: Num a => M3 a b -> [M3 a b] -> M3 a b
distributeFromLeft p ms = Sum2 $ map (\x -> Prod 1 [p,x]) ms


pretty :: (Show a, Num a, Eq a, Show b) => M3 a b -> String
pretty = \case
  Base b -> show b
  Prod 1 ms -> foldr (\x y -> pretty x ++ y) "" ms
  Prod s ms -> show s ++ "(" ++ foldr (\x y -> pretty x ++ y) "" ms ++ ")"
  Sum2 [] -> ""
  Sum2 ms -> foldr1 (\x y -> x ++ "+" ++ y) (map pretty ms)
