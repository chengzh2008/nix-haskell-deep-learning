-- https://crypto.stanford.edu/~blynn/haskell/brain.html

module NeuralNetworkFromScratch.NNFS where

import           Codec.Compression.GZip         ( decompress )
import           Control.Monad
import qualified Data.ByteString.Lazy          as BS
import           Data.Functor
import           Data.Ord
import           Data.List
import           System.Random

width = 28
dimension = 784

rawDataByIndex s n = BS.unpack $ BS.take dimension $ BS.drop (n * dimension) s
-- >>> :t rawDataByIndex
-- rawDataByIndex
--   :: BS.ByteString -> GHC.Int.Int64 -> [GHC.Word.Word8]
--

splitData :: [a] -> [[a]]
splitData as = case length rest of
  0 -> first : []
  _ -> first : splitData rest
 where
  first = take width as
  rest  = drop width as

dealWithBigEndian :: [a] -> [a]
dealWithBigEndian xs = second <> first
 where
  first  = take 14 xs
  second = drop 14 xs

viewData :: (Ord a, Show a, Num a) => [[a]] -> Int -> IO ()
viewData []       _   = return ()
viewData (x : xs) row = do
  if row == 0 || row == 27
    then putStrLn $ take 86 $ repeat '-'
    else do
      putStr "|"
      putStr $ concat $ f <$> dealWithBigEndian x
      putStrLn "|"
  viewData xs (row + 1)
 where
  f n = case n < 10 of
    True  -> "  " <> if n == 0 then " " else show n
    False -> case n < 100 of
      True  -> " " <> show n
      False -> show n

renderNumber s n = viewData (splitData $ rawDataByIndex s n) 0

render n = let s = " .:o0@" in s !! (fromIntegral n * length s `div` 256)

run = do
  -- select a random number as index of the train image
  n <- (`mod` 60000) <$> randomIO
  s <- decompress <$> BS.readFile
    "./src/NeuralNetworkFromScratch/data/train-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile
    "./src/NeuralNetworkFromScratch/data/train-labels-idx1-ubyte.gz"
  print $ (n, BS.index l (n + 8))
  viewData (splitData $ rawDataByIndex s n) 0



-- >>> run
-- (53113,7)
-- --------------------------------------------------------------------------------------
-- |                                                                                    |
-- |                                                                                    |
-- |                                                                                    |
-- |                                                                                    |
-- |                                                                                    |
-- |                                                                                    |
-- |                                                  5145255254249191191 46            |
-- |                                         5 20 36141253253253253253253171            |
-- |                                       128217253253212220253253253243 56            |
-- |                                     88251253253176 15 24117252253173               |
-- |                                  38224253253219 14       40250253 88               |
-- |                                2148253253193 68       40172253177 10               |
-- |                              137253253181109         153253253104                  |
-- |                            34230253253 55          55236253175                     |
-- |                            54253253178  4       10205253253 49                     |
-- |                            38234226 47          88253253174  2                     |
-- |                              119104          13200253224 43                        |
-- |                                              86253253115                           |
-- |                                         4 11201253253 41                           |
-- |                                        90197253253118  1                           |
-- |                                     20245253253246 52                              |
-- |                                    137253253246103                                 |
-- |                                  71247253253127                                    |
-- |                                  89253251196 20                                    |
-- |                                  89253 82 68                                       |
-- |                                                                                    |
-- --------------------------------------------------------------------------------------
--

-- random with stdev 
gauss :: Float -> IO Float
gauss stdev = do
  x1 <- randomIO
  x2 <- randomIO
  return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- activation function
relu = max 0

-- bias for each layer of neurons
type Input = [Float]
type Output = [Float]
type Bias = [Float] -- the ith float is the the bias for the ith neuron
type Weights = [[Float]] -- the ith row holds the weights of the inputs to the ith neoron
type Layer = (Bias, Weights)
type Layers = [Layer] -- the ith in the list represents the ith layer

getLayers :: [Int] -> IO Layers
getLayers szs@(_ : ts) =
  zip (flip replicate 1 <$> ts)
    <$> zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts

-- "3 inputs, a hidden layer of 4 neurons, and 2 output neurons:"
-- >>>  getLayers [3, 4, 2]
-- [([1.0,1.0,1.0,1.0],[[9.907935e-3,1.7610624e-2,8.4903855e-3],[-8.020629e-3,-9.393888e-3,-7.451966e-3],[-4.5380415e-3,9.849725e-3,-2.0997256e-2],[-8.754601e-3,-2.4237656e-3,1.24545e-2]]),([1.0,1.0],[[-2.0274768e-2,-2.8813325e-3,-3.5669326e-3,-7.5384453e-3],[-1.9127443e-2,5.4793264e-4,-1.0574712e-2,-9.191597e-3]])]

-- for each layer 
zLayer :: Input -> (Bias, Weights) -> Output
zLayer as (bs, ws) = zipWith (+) bs $ sum . zipWith (*) as <$> ws

-- forward feed
feed :: Input -> Layers -> Output
feed as layers = foldl' (((relu <$>) .) . zLayer) as layers

-- >>> getLayers [3, 4, 5] >>= print . feed [0.1, 0.2, 0.3]
-- [1.0123188,0.9981441,1.0040364,0.99591523,1.0138805]
--

relu' x | x < 0     = 0
        | otherwise = 1

dCost a y | y == 1 && a >= y = 0
          | otherwise        = a - y

-- xv: vector of inputs
-- Returns a list of (weighted inputs, activations) of each layer,
-- from last layer to first.
revaz :: Input -> Layers -> ([[Float]], [[Float]])
revaz xv = foldl'
  (\(avs@(av : _), zs) (bs, wms) ->
    let zs' = zLayer av (bs, wms) in (((relu <$> zs') : avs), (zs' : zs))
  )
  ([xv], [])

-- xv: vector of inputs
-- yv: vector of desired outputs
-- Returns list of (activations, deltas) of each layer in order.
deltas :: Input -> Output -> Layers -> ([[Float]], [[Float]])
deltas xv yv layers =
  let (avs@(av : _), zv : zvs) = revaz xv layers
      delta0 = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
  in  (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0])
 where
  f _          []         dvs          = dvs
  f (wm : wms) (zv : zvs) dvs@(dv : _) = f wms zvs $ (: dvs) $ zipWith
    (*)
    [ (sum $ zipWith (*) row dv) | row <- wm ]
    (relu' <$> zv)

-- online learning
-- eta = 0.002
-- >>> :t descend
-- descend :: [Double] -> [Double] -> [Double]
--

learn :: Float -> Input -> Output -> Layers -> Layers
learn eta xv yv layers =
  let (avs, dvs) = deltas xv yv layers
  in  zip (zipWith descend (fst <$> layers) dvs) $ zipWith3
        (\wvs av dv -> zipWith (\wv d -> descend wv ((d *) <$> av)) wvs dv)
        (snd <$> layers)
        avs
        dvs
  where descend av dv = zipWith (-) av ((eta *) <$> dv)

getImage s n =
  fromIntegral . BS.index s . (n * dimension + 16 +) <$> [0 .. dimension - 1]
getX s n = (/ 256) <$> getImage s n
getLabel s n = fromIntegral $ BS.index s (n + 8)
getY s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0 .. 9]

-- >>> :t fromEnum
-- >>> fromIntegral . fromEnum . (4 ==) <$> [0 .. 9]
-- fromEnum :: Enum a => a -> Int
-- [0,0,0,0,1,0,0,0,0,0]
--

trainInputgz = "src/NeuralNetworkFromScratch/data/train-images-idx3-ubyte.gz"
trainLabelgz = "src/NeuralNetworkFromScratch/data/train-labels-idx1-ubyte.gz"
testInputgz = "src/NeuralNetworkFromScratch/data/t10k-images-idx3-ubyte.gz"
testLabelsgz = "src/NeuralNetworkFromScratch/data/t10k-labels-idx1-ubyte.gz"

test :: IO ()
test = do
  [trainI, trainL, testI, testL] <- mapM
    ((decompress <$>) . BS.readFile)
    [trainInputgz, trainLabelgz, testInputgz, testLabelsgz]
  -- 30 hidden neurons, 10 output 

  -- initial layers of bias and weights
  layers <- getLayers [784, 30, 10]
  -- randomly choose a number less than 10000
  n      <- (`mod` 10000) <$> randomIO
  -- show the image of the number from test image
  renderNumber testI n
  -- putStr . unlines $ take 28 $ take 28 <$> iterate
  --   (drop 28)
  --   (render <$> getImage testI n)

  let
    example = getX testI n
    -- learning rate
    eta     = 0.002
    bs      = scanl
      (foldl' (\b n -> learn eta (getX trainI n) (getY trainL n) b))
      layers
      [ [0 .. 999]
      , [1000 .. 2999]
      , [3000 .. 5999]
      , [6000 .. 9999]
      , [10000 .. 20000]
      ]
    smart = last bs
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0 ..]

  forM_ bs $ putStrLn . unlines . zipWith cute [0 .. 9] . feed example

  putStrLn $ "best guess: " ++ show (bestOf $ feed example smart)

  let guesses = bestOf . (\n -> feed (getX testI n) smart) <$> [0 .. 9999]
  let answers = getLabel testL <$> [0 .. 9999]
  putStrLn
    $  show (sum $ fromEnum <$> zipWith (==) guesses answers)
    ++ " / 10000"

-- >>> test
-- *** Exception: src/NeuralNetworkFromScratch/train-images-idx3-ubyte.gz: openBinaryFile: does not exist (No such file or directory)
--
