module BackpropAutoDifferentiation.AD where

data Dual = Dual Double Double deriving (Eq, Show)


constD :: Double -> Dual
constD x = Dual x 0

-- >>> constD 2
-- Dual 2.0 0.0


idD :: Double -> Dual
idD x = Dual x 1.0

-- >>> idD 3
-- Dual 3.0 1.0

instance Num Dual where
  fromInteger = constD . fromInteger
  Dual a a' + Dual b b' = Dual (a + b) (a' + b')
  Dual a a' * Dual b b' = Dual (a * b) (a * b' + b * a')
  negate (Dual a a') = Dual (negate a) (negate a')
  abs _ = undefined
  signum _ = undefined

instance Fractional Dual where
  fromRational = constD . fromRational
  recip (Dual a a') = Dual (1.0 / a) (-a' / (a * a))

instance Floating Dual where
  pi = constD pi
  exp (Dual a a') = Dual (exp a) (a' * exp a)
  log (Dual a a') = Dual (log a) (a' / a)
  sqrt (Dual a a') = Dual (sqrt a) (a' / (2 * sqrt a))
  sin (Dual a a') = Dual (sin a) (a' * cos a)
  cos (Dual a a') = Dual (cos a) (a' * (- sin a))
  sinh (Dual a a') = Dual (sinh a) (a' * cosh a)
  cosh (Dual a a') = Dual (cosh a) (a' * sinh a')
  asin (Dual a a') = Dual (asin a) (a' / sqrt (1 - a * a))
  acos (Dual a a') = Dual (acos a) (a' / (-sqrt (1 - a * a)))
  atan (Dual a a') = Dual (atan a) (a' / (1 + a * a))
  asinh (Dual a a') = Dual (asinh a) (a' / sqrt (1 + a * a))
  acosh (Dual a a') = Dual (acosh a) (a' / (sqrt (a * a - 1)))
  atanh (Dual a a') = Dual (atanh a) (a' / (1 - a * a))

f :: Floating a => a -> a
f = sqrt . (* 3) . sin

f' :: Floating a => a -> a
f' x = 3 * cos x / (2 * sqrt (3 * sin x))

-- >>> :t f
-- f :: Floating a => a -> a
--

-- calculate value and the derivative at 2
-- >>> f $ idD 2
-- Dual 1.6516332160855343 (-0.3779412091869595)

-- >>> f' 2
-- -0.3779412091869595
--


