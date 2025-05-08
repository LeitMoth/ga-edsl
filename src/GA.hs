-- References:
-- https://geometrica.vialattea.net/en/the-rotor/
-- https://rigidgeometricalgebra.org/wiki/index.php?title=Reverses
-- https://jacquesheunis.com/post/rotors/
-- https://en.wikipedia.org/wiki/Geometric_algebra

-- Try:
-- ghci> b = bnorm ((e1+e2) /\ (e2+e3))
-- ghci> plot [] $ rotate b (t*tau) e1

module GA
  ( e1,
    e2,
    e3,
    e12,
    e23,
    e31,
    e21,
    e32,
    e13,
    e123,
    i,
    var,
    dot,
    wedge,
    (/\),
    norm,
    bnorm,
    brev,
    binv,
    tau,
    rotor,
    rotate,
    eval,
    plot,
    t,
  )
where

import Data.Function (on)
import Data.List (find, partition)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Graphics.Gnuplot.Simple (plotMesh3d)

--------------------------------------------------------------------------------
--------------------------- Graphing -------------------------------------------
--------------------------------------------------------------------------------

offset :: (Fractional a) => (a, a, a) -> (a, a, a)
offset (x, y, z) = (x, y, z + 0.01)

thicken :: (Fractional a) => [(a, a, a)] -> [[(a, a, a)]]
thicken points = [points, map offset points]

t :: M2 a
t = var "t"

plot :: [(String, M2 Double)] -> M2 Double -> IO ()
plot vars thing = plotMesh3d [] [] graphdata
  where
    f time = eval (("t", Scal time) : vars) thing
    points = map (vector . f) [0, 0.01 .. 1]
    graphdata = thicken points

vector :: (Num a, Eq a) => M2 a -> (a, a, a)
vector m = (x, y, z)
  where
    M4 m4 = toM4 m
    findBasis b = maybe 0 scalarPart (find (((==) `on` nonScalarPart) [ABase b]) m4)
    x = findBasis X
    y = findBasis Y
    z = findBasis Z

--------------------------------------------------------------------------------
--------------------------- AST and common operations  -------------------------
--------------------------------------------------------------------------------

data Func = Exp | Inv
  deriving (Eq, Show)

-- | M2 represents a multivector.
--   This is built when the user types in input,
--   and is later processed to M4 if we need a multivector
--   in a normalized form.
data M2 a where
  Scal :: a -> M2 a
  Basis1 :: M2 a
  Basis2 :: M2 a
  Basis3 :: M2 a
  Mul :: M2 a -> M2 a -> M2 a
  Sum :: M2 a -> M2 a -> M2 a
  Func :: Func -> M2 a -> M2 a
  Var :: String -> M2 a

instance (Ord a, Eq a, Num a, Show a) => Show (M2 a) where
  show = pretty

instance (Fractional a) => Fractional (M2 a) where
  fromRational = Scal . fromRational
  recip = \case
    Scal a -> Scal (1 / a)
    b -> Func Inv b

unimplementedTrig :: String
unimplementedTrig = "Only pi and exp work, trig functions don't"

instance (Floating a, Eq a) => Floating (M2 a) where
  pi = Scal pi
  exp = Func Exp
  sqrt (Scal x) = Scal (sqrt x)
  sqrt x = Scal (sqrt (resolveScalar (toM4 x)))
  log = error unimplementedTrig
  sin = error unimplementedTrig
  cos = error unimplementedTrig
  asin = error unimplementedTrig
  acos = error unimplementedTrig
  atan = error unimplementedTrig
  sinh = error unimplementedTrig
  cosh = error unimplementedTrig
  asinh = error unimplementedTrig
  acosh = error unimplementedTrig
  atanh = error unimplementedTrig

tau :: (Floating a, Eq a) => M2 a
tau = 2 * pi

type Ctx a = M.Map String (M2 a)

interp :: (Fractional a, Eq a) => Ctx a -> M2 a -> M2 a
interp ctx = \case
  Mul a b -> Mul (interp ctx a) (interp ctx b)
  Sum a b -> Sum (interp ctx a) (interp ctx b)
  Func f a -> case f of
    Exp -> taylorExp 16 a'
    Inv -> Scal $ resolveScalar (toM4 a')
    where
      a' = interp ctx a
  Var name -> ctx M.! name
  a -> a

factorial :: Integer -> Integer
factorial n = product [1 .. n]

pow :: (Num a) => Integer -> M2 a -> M2 a
pow n m = foldr Mul (Scal 1) (replicate (fromEnum n) m)

taylorExp :: (Fractional a) => Integer -> M2 a -> M2 a
taylorExp 0 _ = 1
taylorExp level m =
  Sum
    (taylorExp (level - 1) m)
    (Mul (Scal (1 / fromInteger (factorial level))) (pow level m))

eval :: (Fractional a, Eq a) => [(String, M2 a)] -> M2 a -> M2 a
eval vars = interp (M.fromList vars)

brev :: (Num a) => M2 a -> M2 a
brev = negate

binv :: (Fractional a) => M2 a -> M2 a
binv b = Mul (brev b) (Func Inv (Mul b (brev b)))

rotor :: (Floating a, Eq a) => M2 a -> M2 a -> M2 a
rotor plane angle = exp (-(plane * (angle / 2)))

rotate :: (Floating a, Eq a) => M2 a -> M2 a -> M2 a -> M2 a
rotate plane angle x = b * x * binv b
  where
    b = rotor plane angle

dot :: (Fractional a) => M2 a -> M2 a -> M2 a
dot a b = 0.5 * (a * b + b * a)

wedge :: (Fractional a) => M2 a -> M2 a -> M2 a
wedge a b = 0.5 * (a * b - b * a)

(/\) :: (Fractional a) => M2 a -> M2 a -> M2 a
(/\) = wedge

norm :: (Floating a, Eq a) => M2 a -> M2 a
norm a = a / sqrt (dot a a)

bnorm :: M2 Double -> M2 Double
bnorm b = b / sqrt (dot (e123 * b) (e123 * b))

e1 :: M2 Double
e1 = Basis1

e2 :: M2 Double
e2 = Basis2

e3 :: M2 Double
e3 = Basis3

e12 :: M2 Double
e12 = Mul Basis1 Basis2

e23 :: M2 Double
e23 = Mul Basis2 Basis3

e31 :: M2 Double
e31 = Mul Basis3 Basis1

e21 :: M2 Double
e21 = Mul Basis2 Basis1

e32 :: M2 Double
e32 = Mul Basis3 Basis2

e13 :: M2 Double
e13 = Mul Basis1 Basis3

e123 :: M2 Double
e123 = Mul (Mul Basis1 Basis2) Basis3

i :: M2 Double
i = e123

var :: String -> M2 a
var = Var

instance (Num a) => Num (M2 a) where
  (+) = Sum
  (*) = Mul
  abs = error "abs not implemented"
  signum = error "signum not implemented"
  fromInteger = Scal . fromInteger
  negate = Mul (Scal (-1))

--------------------------------------------------------------------------------
--------------------------- Normalized Representation --------------------------
--------------------------------------------------------------------------------

data Basis = X | Y | Z
  deriving (Eq, Ord)

instance Show Basis where
  show = \case
    X -> "e1"
    Y -> "e2"
    Z -> "e3"

-- | The idea of the M4 representation is similar to Disjunctive Normal Form in
--   boolean logic. The outer list reprents a sum, and each inner list
--   represents a product. Normalizing the above AST into this form
--   is a good way to simplify before pretty-printing, and allows
--   for nicities like grouping like terms.
data M4 a b where
  M4 :: [[Atom a b]] -> M4 a b
  deriving (Eq, Show)

data Atom a b where
  ABase :: b -> Atom a b
  AScalar :: a -> Atom a b
  AFunc :: Func -> M4 a b -> Atom a b
  AVar :: String -> Atom a b
  deriving (Eq, Show)

-- | Multiplies two M4 representations, handling distributivity
--   to maintain the "disjusnctive normal form" style.
--   Looking at the code doesn't give a great impression
--   of how it works, but trying it on a few examples
--   should clear things up quickly.
m4mult :: (Num a, Eq a) => M4 a b -> M4 a b -> M4 a b
m4mult (M4 a) (M4 b) = M4 $ map concat (sequence [a, b])

m4add :: M4 a b -> M4 a b -> M4 a b
m4add (M4 a) (M4 b) = M4 $ a ++ b

toM4 :: (Num a, Eq a) => M2 a -> M4 a Basis
toM4 = simplify . toM4Helper

-- Because basisCollapse is linear, it struggles
-- with things like e123e123, in which the second e1
-- needs to move backwards 2 places to reach e1^2 and simplify.
-- The is a flaw, and basisCollapse needs to be rewritten into
-- a proper sorting algorithm.
-- However, in my testing, the expressions I used
-- never needed more than 2 stages of simplification.
-- Here I do 4 to be safe, this works well for now.
simplify :: (Num a, Eq a, Ord b) => M4 a b -> M4 a b
simplify = simplifyHelper . simplifyHelper . simplifyHelper . simplifyHelper

simplifyHelper :: (Num a, Eq a, Ord b) => M4 a b -> M4 a b
simplifyHelper = likeTermCollapse . m4map scalarCollapse . m4map basisCollapse

toM4Helper :: (Num a, Eq a) => M2 a -> M4 a Basis
toM4Helper = \case
  Scal a -> M4 [[AScalar a]]
  Basis1 -> M4 [[ABase X]]
  Basis2 -> M4 [[ABase Y]]
  Basis3 -> M4 [[ABase Z]]
  Mul a b -> simplify $ (m4mult `on` toM4) a b
  Sum a b -> simplify $ (m4add `on` toM4) a b
  Func f a -> M4 [[AFunc f (toM4 a)]]
  Var name -> M4 [[AVar name]]

m4map :: ([Atom a b] -> [Atom a b]) -> M4 a b -> M4 a b
m4map f (M4 dnf) = M4 (map f dnf)

resolveScalar :: (Num a, Eq a) => M4 a b -> a
resolveScalar = \case
  M4 m4 -> scalarPart (head m4)

-- | Multiplies all the scalars in one term together,
--   and neatly places the resulting scaler at the front of the term.
scalarCollapse :: (Num a, Eq a) => [Atom a b] -> [Atom a b]
scalarCollapse as = list'
  where
    (scalars, list) = partition (\case AScalar _ -> True; _ -> False) as
    scalar = product $ map (\case AScalar a -> a; _ -> error "BUG, expected scalar") scalars
    list' =
      if scalar == 1 && not (null list)
        then list
        else AScalar scalar : list

-- | Finds like terms within an M4 and groups them together,
--   simplifying the overall expression.
likeTermCollapse :: (Num a, Eq a, Eq b) => M4 a b -> M4 a b
likeTermCollapse (M4 ms) = M4 (likeTermCollapseHelper ms)

likeTermCollapseHelper :: (Num a, Eq a, Eq b) => [[Atom a b]] -> [[Atom a b]]
likeTermCollapseHelper [] = []
likeTermCollapseHelper (p : ps) = list
  where
    (likeTerms, ps') = partition (((==) `on` nonScalarPart) p) (p : ps)
    total = sum $ map scalarPart likeTerms
    ps'' = likeTermCollapseHelper ps'
    list =
      if total /= 1
        then (AScalar total : nonScalarPart p) : ps''
        else p : ps''

nonScalarPart :: [Atom a b] -> [Atom a b]
nonScalarPart = filter (\case AScalar _ -> False; _ -> True)

scalarPart :: (Num a, Eq a) => [Atom a b] -> a
scalarPart = fromMaybe 1 . scalarPartMaybe

scalarPartMaybe :: (Num a, Eq a) => [Atom a b] -> Maybe a
scalarPartMaybe as = do
  front <- listToMaybe $ scalarCollapse as
  case front of
    AScalar a -> Just a
    _ -> Nothing

-- | Handles the "multiplication table" of basis vectors
--   that determines the nature of the geometric algebra.
--   Here we are assuming vector geometric algebra
--   in 3 dimensions.
basisCollapse :: (Num a, Ord b) => [Atom a b] -> [Atom a b]
basisCollapse as = AScalar parity : list
  where
    (list, parity) = basisCollapseHelper as

basisCollapseHelper :: (Num a, Ord b) => [Atom a b] -> ([Atom a b], a)
basisCollapseHelper (ABase b1 : ABase b2 : rest) =
  case compare b1 b2 of
    GT -> (ABase b2 : tailing, parity')
      where
        (tailing, parity) = basisCollapseHelper (ABase b1 : rest)
        parity' = -parity
    EQ -> basisCollapseHelper rest -- Basis squared is 1
    LT -> (ABase b1 : tailing, parity')
      where
        (tailing, parity) = basisCollapseHelper (ABase b2 : rest)
        parity' = parity
basisCollapseHelper (m1 : m2 : rest) =
  (m1 : tailing, parity')
  where
    (tailing, parity) = basisCollapseHelper (m2 : rest)
    parity' = parity
basisCollapseHelper [] = ([], 1)
basisCollapseHelper [m] = ([m], 1)

--------------------------------------------------------------------------------
----------------------------- Pretty Printing ----------------------------------
--------------------------------------------------------------------------------

joinOp :: String -> [String] -> String
joinOp op = foldr1 (\x y -> x ++ op ++ y)

prettyM4 :: (Ord a, Show a, Num a, Eq a, Show b) => M4 a b -> String
prettyM4 (M4 []) = "0"
prettyM4 (M4 ms) = joinOp " + " $ map prettyProduct ms

-- Most of the work is done by the normalized form.
-- However, negation caused some trouble when trying to
-- copy and paste the pretty printed output back in.
-- So I had to add a large amount of logic to
-- put parentheses around negative numbers.
prettyProduct :: (Ord a, Show a, Num a, Eq a, Show b) => [Atom a b] -> String
prettyProduct [] = "1"
prettyProduct as = full
  where
    s = scalarPart as
    rest = nonScalarPart as
    full =
      case compare s 0 of
        EQ -> "0"
        GT ->
          if s == 1
            then
              if null rest
                then "1"
                else joinOp "*" $ map prettyAtom rest
            else joinOp "*" $ map prettyAtom as
        LT ->
          if null rest
            then "(" ++ show s ++ ")"
            else
              if s == -1
                then "(-1)" ++ "*" ++ joinOp "*" (map prettyAtom rest)
                else "(" ++ show s ++ ")" ++ "*" ++ joinOp "*" (map prettyAtom rest)

prettyAtom :: (Ord a, Show a, Num a, Eq a, Show b) => Atom a b -> String
prettyAtom = \case
  ABase b -> show b
  AScalar s -> show s
  AFunc Exp m -> "exp(" ++ prettyM4 m ++ ")"
  AFunc Inv m -> "(" ++ prettyM4 m ++ ")^-1"
  AVar name -> name

pretty :: (Ord a, Show a, Num a, Eq a) => M2 a -> String
pretty = prettyM4 . toM4
