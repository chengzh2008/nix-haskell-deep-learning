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

-- calculate value and the derivative at 2
-- >>> f $ idD 2
-- Dual 1.6516332160855343 (-0.3779412091869595)

-- >>> f' 2
-- -0.3779412091869595

-- Application

-- cost function 
cost m c xs ys = (/ (2 * (fromIntegral $ length xs))) $ sum $ zipWith errSq
                                                                      xs
                                                                      ys
  where errSq x y = z * z where z = y - (m * x + c)


-- >>> :t cost
-- cost :: Fractional a => a -> a -> [a] -> [a] -> a
--

xs, ys :: (Fractional a) => [a]
xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
ys = [3, 5, 7, 9, 11, 13, 15, 17, 19, 21]
-- >>> :t xs
-- xs :: Fractional a => [a]
--

-- learning rate
gamma = 0.04

-- function of params m and c
g :: (Fractional a) => a -> a -> a
g m c = cost m c xs ys
-- >>> :t g
-- g :: Fractional a => a -> a -> a
--

-- use Dual numbers to calculate the rquired partial derivatives and updat our estimates of the parameter
zs = (0.1, 0.1) : map f zs
 where
  deriv (Dual _ x') = x'
  f (c, m) = (c - gamma * cDeriv, m - gamma * mDeriv)
   where
    cDeriv = deriv $ g (constD m) $ idD c
    mDeriv = deriv $ flip g (constD c) $ idD m
-- >>> :t zs
-- zs :: [(Double, Double)]
--

-- >>> take 2 $ drop 1000 $ zs
-- [(0.9998665320141327,2.0000191714150106),(0.999867653022265,2.0000190103927853)]
--
-- >>> take 2 $ drop 1000 $ map (\(c, m) -> cost m c xs ys) zs
-- [1.9088215184565296e-9,1.876891490619424e-9]
--

-- sqrt of n 
sq n = 1 : map f (sq n) where f guess = (n / guess + guess) / 2
-- >>> take 5 $ sq 2
-- [1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899]
--
-- >>> take 5 $ sq 3
-- [1.0,2.0,1.75,1.7321428571428572,1.7320508100147274]
--
