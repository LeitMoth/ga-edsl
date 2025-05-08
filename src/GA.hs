{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GA () where

import Data.Function (on)
import Data.List (partition)

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
  Var :: String -> M2 a
  deriving (Show)

e1 = Basis1

e2 = Basis2

e3 = Basis3

mexp :: (Floating a) => M2 a -> M2 a
mexp = Func Exp

var :: String -> M2 a
var = Var

instance Functor M2 where
  fmap f (Scal a) = Scal (f a)
  fmap f (Mul a b) = Mul (fmap f a) (fmap f b)
  fmap f (Sum a b) = Sum (fmap f a) (fmap f b)
  fmap _ Basis1 = Basis1
  fmap _ Basis2 = Basis2
  fmap _ Basis3 = Basis3

instance (Num a) => Num (M2 a) where
  (+) = Sum
  (*) = Mul
  abs = error "not implemented"
  signum = error "not implemented"
  fromInteger = Scal . fromInteger
  negate = fmap negate

-- simplify :: (Num a) => M2 a -> M2 a
-- simplify = \case
--   Scal a -> Scal a
--   Basis1 -> Basis1
--   Basis2 -> Basis2
--   Basis3 -> Basis3
--   Mul a (Mul b c) -> fixup $ Mul (Mul a b) c
--   Mul Basis2 Basis1 -> Mul (-1) (Mul Basis1 Basis2)
--   Mul Basis1 Basis3 -> Mul (-1) (Mul Basis3 Basis1)
--   Mul Basis3 Basis2 -> Mul (-1) (Mul Basis2 Basis3)
--   Mul Basis1 Basis1 -> Scal 1
--   Mul Basis2 Basis2 -> Scal 1
--   Mul Basis3 Basis3 -> Scal 1
--   Mul a b -> fixup (Mul (simplify a) (simplify b))
--   Sum a b -> fixup (Sum (simplify a) (simplify b))
--
-- fixup :: (Num a) => M2 a -> M2 a
-- fixup = orderBasis . pullScalar
--
-- pullScalar :: (Num a) => M2 a -> M2 a
-- pullScalar (Mul (Scal a) (Scal b)) = Scal (a * b)
-- pullScalar (Mul a (Scal b)) = pullScalar (Mul (Scal b) a)
-- pullScalar (Mul (Mul (Scal a) b) c) = Mul (Scal a) (Mul b c)
-- pullScalar (Mul c (Mul (Scal a) b)) = Mul (Scal a) (Mul b c)
-- pullScalar a = a
--
-- orderBasis :: (Num a) => M2 a -> M2 a
-- orderBasis = \case
--   Mul Basis2 Basis1 -> Mul (-1) (Mul Basis1 Basis2)
--   Mul Basis1 Basis3 -> Mul (-1) (Mul Basis3 Basis1)
--   Mul Basis3 Basis2 -> Mul (-1) (Mul Basis2 Basis3)
--   Mul (Mul Basis2 Basis3) Basis1 -> Mul (Mul Basis1 Basis2) Basis3
--   Mul a b -> Mul (orderBasis a) (orderBasis b)
--   a -> a
--
-- data M3 a b where
--   Base :: b -> M3 a b
--   Prod :: a -> [M3 a b] -> M3 a b
--   Sum2 :: [M3 a b] -> M3 a b
--   deriving (Show)
--
-- funmap :: (M3 a b -> M3 a b) -> M3 a b -> M3 a b
-- funmap f (Base b) = Base b
-- funmap f (Prod s ms) = Prod s (map f ms)
-- funmap f (Sum2 ms) = Sum2 (map f ms)

data Basis = X | Y | Z
  deriving (Eq, Ord)

instance Show Basis where
  show = \case
    X -> "e1"
    Y -> "e2"
    Z -> "e3"

-- broil :: (Num a) => M2 a -> M3 a Basis
-- broil = \case
--   Scal a -> Prod a []
--   Basis1 -> Base X
--   Basis2 -> Base Y
--   Basis3 -> Base Z
--   Mul a b -> Prod 1 [broil a, broil b]
--   Sum a b -> Sum2 [broil a, broil b]
--
-- -- distribute
--
-- bake :: (Num a) => M3 a b -> M3 a b
-- bake = \case
--   Prod a ((Prod b bs) : cs) -> bake $ Prod (a * b) (map bake bs ++ map bake cs)
--   Sum2 ((Sum2 bs) : cs) -> bake $ Sum2 (bs ++ cs)
--   a -> a
--
-- -- normalize :: Num a => M3 a b -> M3 a b
-- -- normalize = pullProd
--
-- pullProd :: (Num a) => M3 a b -> M3 a b
-- pullProd = \case
--   Prod s ms -> foldr f (Prod s []) ms
--     where
--       f a (Prod s1 ms1) = case pullProd a of
--         Prod s2 ms2 -> Prod (s1 * s2) (ms2 ++ ms1)
--         m -> Prod s1 (m : ms1)
--       f _ _ = error "BUG, should always be a Prod"
--   Sum2 ms -> funmap pullProd (pullSum (Sum2 ms))
--   a -> a
--
-- pullSum :: (Num a) => M3 a b -> M3 a b
-- pullSum = \case
--   Sum2 ms -> foldr f (Sum2 []) ms
--     where
--       f a (Sum2 ms1) = case a of
--         Sum2 ms2 -> Sum2 (ms2 ++ ms1)
--         m -> Sum2 (m : ms1)
--       f _ _ = error "BUG, should always be a Sum2"
--   a -> a
--
-- distribute :: (Num a) => M3 a b -> M3 a b
-- distribute = \case
--   Base b -> Base b
--   Sum2 ms -> Sum2 ms
--   Prod s ms -> foldr f (Prod 1 [Sum2 []]) ms
--     where
--       f a (Prod _ [Sum2 ms1]) = case a of
--         Sum2 ms2 -> Sum2 (ms2 ++ ms1)
--         m -> Sum2 (m : ms1)
--       f _ _ = error "BUG, should always be a Sum2"
--
-- distributeFromRight :: (Num a) => a -> [M3 a b] -> M3 a b -> M3 a b
-- distributeFromRight ambient ms p = Sum2 $ map (\x -> Prod ambient [x, p]) ms
--
-- distributeFromLeft :: (Num a) => a -> M3 a b -> [M3 a b] -> M3 a b
-- distributeFromLeft ambient p ms = Sum2 $ map (\x -> Prod ambient [p, x]) ms
--
-- distribute2 :: (Num a) => a -> [M3 a b] -> [M3 a b]
-- distribute2 ambient = \case
--   ((Sum2 ms) : p2 : rest) -> distribute2 ambient (distributeFromRight ambient ms p2 : rest)
--   (p1 : (Sum2 ms) : rest) -> distribute2 ambient (distributeFromLeft ambient p1 ms : rest)
--   (m1 : m2 : rest) -> distribute2 ambient $ flatten [m1, m2] : rest
--   [Sum2 ms] -> ms
--   [m] -> [m]
--   [] -> []
--
-- flatten :: (Num a) => [M3 a b] -> M3 a b
-- flatten = \case
--   [] -> Prod 0 []
--   (Prod s1 ms1 : Prod s2 ms2 : rest) -> (flatten (Prod (s1 * s2) (ms1 ++ ms2) : rest))
--   [Prod s1 ms1] -> Prod s1 ms1
--   (p : rest) -> flatten (Prod 1 [p] : rest)

-- ghci> map (foldr (++) []) $ sequence $ [a,b]

data M4 a b where
  M4 :: [[Atom a b]] -> M4 a b
  deriving (Show)

data Atom a b where
  Base2 :: b -> Atom a b
  Scalar2 :: a -> Atom a b
  Func2 :: FuncThing -> M4 a b -> Atom a b
  Var2 :: String -> Atom a b
  deriving (Show)

m4mult :: (Num a, Eq a) => M4 a b -> M4 a b -> M4 a b
m4mult (M4 a) (M4 b) = M4 $ map concat (sequence [a, b])

-- frontCleanup :: (Num a, Eq a) => [Atom a b] -> [Atom a b]
-- frontCleanup (Scalar2 s1 : Scalar2 s2 : rest) = frontCleanup (Scalar2 (s1 * s2) : rest)
-- frontCleanup (Scalar2 s1 : rest) =
--   if s1 == 1
--     then rest
--     else Scalar2 s1 : rest
-- frontCleanup as = as

m4add :: M4 a b -> M4 a b -> M4 a b
m4add (M4 a) (M4 b) = M4 $ a ++ b

toM4 :: (Num a, Eq a) => M2 a -> M4 a Basis
toM4 = simplify . toM4Helper

simplify :: (Num a, Eq a, Ord b) => M4 a b -> M4 a b
simplify = m4map scalarCollapse . m4map basisCollapse

toM4Helper :: (Num a, Eq a) => M2 a -> M4 a Basis
toM4Helper = \case
  Scal a -> M4 [[Scalar2 a]]
  Basis1 -> M4 [[Base2 X]]
  Basis2 -> M4 [[Base2 Y]]
  Basis3 -> M4 [[Base2 Z]]
  Mul a b -> simplify $ (m4mult `on` toM4) a b
  Sum a b -> simplify $ (m4add `on` toM4) a b
  Func f a -> M4 [[Func2 f (toM4 a)]]
  Var name -> M4 [[Var2 name]]

m4map :: ([Atom a b] -> [Atom a b]) -> M4 a b -> M4 a b
m4map f (M4 dnf) = M4 (map f dnf)

scalarCollapse :: (Num a, Eq a) => [Atom a b] -> [Atom a b]
scalarCollapse as = list'
  where
    (scalars, list) = partition (\case Scalar2 _ -> True; _ -> False) as
    scalar = product $ map (\case Scalar2 a -> a; _ -> error "BUG, expected scalar") scalars
    list' = if scalar == 1 then list else Scalar2 scalar : list

-- likeTermCollapse :: [[Atom a b]] -> [[Atom a b]]
-- likeTermCollapse (p:ps) =


basisCollapse :: (Num a, Ord b) => [Atom a b] -> [Atom a b]
basisCollapse as = Scalar2 parity : list
  where
    (list, parity) = basisCollapseHelper as

basisCollapseHelper :: (Num a, Ord b) => [Atom a b] -> ([Atom a b], a)
basisCollapseHelper (Base2 b1 : Base2 b2 : rest) =
  case compare b1 b2 of
    GT -> (Base2 b2 : tailing, parity')
      where
        (tailing, parity) = basisCollapseHelper (Base2 b1 : rest)
        parity' = -parity
    EQ -> basisCollapseHelper rest -- Basis squared is 1
    LT -> (Base2 b1 : tailing, parity')
      where
        (tailing, parity) = basisCollapseHelper (Base2 b2 : rest)
        parity' = parity
basisCollapseHelper (m1 : m2 : rest) =
  (m1 : tailing, parity')
  where
    (tailing, parity) = basisCollapseHelper (m2 : rest)
    parity' = parity
basisCollapseHelper [] = ([], 1)
basisCollapseHelper [m] = ([m], 1)

-- normalize :: (Num a) => M3 a b -> M3 a b
-- normalize (Prod s ms) = Sum2 (distribute2 s ms)
-- normalize (Sum2 ms) = pullSum $ Sum2 (map normalize ms)
-- normalize (Base b) = Sum2 [Base b]
--
-- pretty :: (Show a, Num a, Eq a, Show b) => M3 a b -> String
-- pretty = \case
--   Base b -> show b
--   Prod 1 ms -> foldr (\x y -> pretty x ++ y) "" ms
--   Prod s ms -> show s ++ "(" ++ foldr (\x y -> pretty x ++ y) "" ms ++ ")"
--   Sum2 [] -> ""
--   Sum2 ms -> foldr1 (\x y -> x ++ "+" ++ y) (map pretty ms)

joinOp :: String -> [String] -> String
joinOp op = foldr1 (\x y -> x ++ op ++ y)

prettyM4 :: (Show a, Num a, Eq a, Show b) => M4 a b -> String
prettyM4 (M4 []) = "0"
prettyM4 (M4 ms) = joinOp " + " $ map prettyProduct ms

prettyProduct :: (Show a, Num a, Eq a, Show b) => [Atom a b] -> String
prettyProduct [] = "1"
prettyProduct as = joinOp "*" $ map prettyAtom as

prettyAtom :: (Show a, Num a, Eq a, Show b) => Atom a b -> String
prettyAtom = \case
  Base2 b -> show b
  Scalar2 s -> show s
  Func2 Exp m -> "mexp(" ++ prettyM4 m ++ ")"
  Var2 name -> name
