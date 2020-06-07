-- https://crypto.stanford.edu/~blynn/haskell/brain.html

module NeuralNetworkFromScratch.NNFS where

import           Codec.Compression.GZip         ( decompress )
import qualified Data.ByteString.Lazy          as BS
import           Data.Functor
import           System.Random

width = 28
dimension = 784

rawDataByIndex s n = BS.unpack $ BS.take dimension $ BS.drop (n * dimension) s

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


render n = let s = " .:o0@" in s !! (fromIntegral n * length s `div` 256)

-- >>> render 33 
-- ' '
--

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
-- (10720,8)
-- --------------------------------------------------------------------------------------
-- |                                                                                    |
-- |                                                                                    |
-- |                                                                                    |
-- |                                         9                                          |
-- |                                       165131214255231 68                           |
-- |                                     56249254253253254251116  2                     |
-- |                                    132253253253253253253253 94                     |
-- |                                   9205253254125 35 58227253215                     |
-- |                                  27245253254 36       47253246 51                  |
-- |                                    184253167  1       16253254 74                  |
-- |                                    129253159          16253254 74                  |
-- |                                     54248248 20       31253254 74                  |
-- |                                       161254141      152253244 47                  |
-- |                                         8253253208150238253209                     |
-- |                                          163251253253253253 49                     |
-- |                                        70 23174253254253253 53                     |
-- |                                   9104249206253253242253253215  5                  |
-- |                                8170254254253240141 20160253253 74                  |
-- |                               76253253194197 65       66253254 74                  |
-- |                               76254254 50 18          15253253 74                  |
-- |                               76253254243        1 59220253240 35                  |
-- |                               33160241254190189191254253253119                     |
-- |                                     73153254254254254227 82  2                     |
-- |                                          153252212130 30                           |
-- |                                                                                    |
-- |                                                                                    |
-- --------------------------------------------------------------------------------------
--
