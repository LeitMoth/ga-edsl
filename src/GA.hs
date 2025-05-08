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

instance (Num a) => Num (M2 a) where
  (+) = Sum
  (*) = Mul
  abs = error "abs not implemented"
  signum = error "signum not implemented"
  fromInteger = Scal . fromInteger
  negate = Mul (Scal (-1))

data Basis = X | Y | Z
  deriving (Eq, Ord)

instance Show Basis where
  show = \case
    X -> "e1"
    Y -> "e2"
    Z -> "e3"

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
