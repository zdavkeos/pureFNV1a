-- |Tests for pureFNV1a module

import Data.Word
import Text.Printf (printf)
import Test.QuickCheck
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.Exit (exitFailure, exitSuccess)
import Control.Monad

import Data.Digest.Pure.FNV1a

type HashTest = (String, [Word8], String)

samples :: [HashTest]
samples = [("empty", [], "2166136261"),
           ("1234", [1,2,3,4], "1463068797"), 
           ("a", [97], "3826002220"),
           ("foo", [102,111,111], "2851307223"),
           ("foobar", [102, 111, 111, 98, 97, 114], "3214735720")
          ]

testSample :: HashTest -> IO Bool
testSample (name, bs, ans) = do
  let res = (show . fnv1a . L.pack) bs == ans
  if res then 
      putStrLn ("Passed: " ++ name) >> return True else
      putStrLn ("Failed: " ++ name) >> return False

--testSamples :: IO Bool
--testSamples = return $ and $ map testSample samples

prop_parts :: [Word8] -> [Word8] -> Bool
prop_parts xs ys = let xs' = B.pack xs
                       ys' = B.pack ys
                       xys = B.append xs' ys'
                       xysl = L.append (L.pack xs) (L.pack ys)
                       r1 = fnv1a xysl
                       r2 = fnv1aFinalize fnv1aInitialContext xys
                       r3 = fnv1aFinalize (fnv1aUpdate fnv1aInitialContext xs') ys'
		   in r1 == r2 && r2 == r3

tests = [("parts", prop_parts)] -- pop_tarts?

main = do
  mapM_ (\(n, t) -> printf "%-25s: " n >> quickCheck t) tests
  res <- liftM and $ mapM testSample samples
  if not res then exitFailure else exitSuccess

