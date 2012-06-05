-- |Read data from stdin, print FNV-1a hash to stdout

import Text.Printf (printf)
import qualified Data.ByteString.Lazy as L

import Data.Digest.Pure.FNV1a

main :: IO ()
main = do
  c <- L.getContents
  let hsh = fnv1a c
  putStrLn $ show hsh

