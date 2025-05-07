module GA (someFunc) where

data Multivector = V
  { s :: Double,
    e1 :: Double,
    e2 :: Double,
    e3 :: Double,
    e12 :: Double,
    e23 :: Double,
    e31 :: Double,
    e123 :: Double
  }


data Multivector where
    scalar :: Double -> Multivector
    e1 :: Double -> Multivector
    e2 :: Double -> Multivector
    e3 :: Double -> Multivector
    e12 :: Double -> Multivector
    e23 :: Double -> Multivector
    e31 :: Double -> Multivector
    e123 :: Double -> Multivector
    lincomb :: [Multivector]

data Multivector where
    one :: Multivector
    e1 :: Multivector
    e2 :: Multivector
    e3 :: Multivector
    e12 :: Multivector
    e23 :: Multivector
    e31 :: Multivector
    e123 :: Multivector
    lincomb :: [(Double,Multivector)]

type Scalar = Double

data Multivector where
    e1 :: Multivector
    e2 :: Multivector
    e3 :: Multivector
    e12 :: Multivector
    e23 :: Multivector
    e31 :: Multivector
    e123 :: Multivector
    lincomb :: [Either Double (Double,Multivector)]


data Multivector = V
  { scalar :: Scalar,
    vector :: Vector,
    bivector :: Bivector,
    trivector :: Trivector
  }

-- data MultiBasis = B
--   { b1 :: Double,
--     b2 :: Double,
--     b3 :: Double
--   }

(.*) :: Multivector -> Multivector -> Multivector
(.*)
  ( V
      { s = as,
        e1 = ae1,
        e2 = ae2,
        e3 = ae3,
        e12 = ae12,
        e23 = ae23,
        e31 = ae31,
        e123 = ae123
      }
    )
  ( V
      { s = bs,
        e1 = be1,
        e2 = be2,
        e3 = be3,
        e12 = be12,
        e23 = be23,
        e31 = be31,
        e123 = be123
      }
    ) =
    V
      { s = as * bs ,
        e1 = 
      }




someFunc :: IO ()
someFunc = putStrLn "someFunc"
