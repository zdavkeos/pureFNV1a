{-# LANGUAGE MultiParamTypeClasses #-}
-- 
-- Module: FNV1a
-- License: BSD3
-- Maintainer: zdavkeos@gmail.com
-- Stability: experimental
-- Portability: portable, requires ByteString
--
-- |Haskell implementation of the FNV-1a hash algorithm
-- It is recommended that you use the crypto-api class based
-- method of accessing the FNV-1a hash algorithm.
--
-- Example:
-- 
-- @
-- print $ fnv1a (pack [1,2,3,4])
-- ==>
-- 3282719157
-- @
-- 
-- Or via the the crypto-api:
-- 
-- @
-- import qualified Data.ByteString as B
-- import Data.Digest.Pure.FNV1a
-- ...
-- hashFile = liftM hash B.readFile
-- @
-- 

module Data.Digest.Pure.FNV1a 
    (
    -- * Data types
    FNV1aDigest,
    FNV1aContext,
    -- * API functions
    fnv1a,
    fnv1aUpdate,
    fnv1aInitialContext,
    fnv1aFinalize,
    -- * Crypo-API
    Hash(..)
    ) where

import Data.Bits
import Data.List
import Data.Word
import Data.Tagged
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString as B
import Data.ByteString.Lazy as L
import qualified Data.Serialize as S
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Crypto.Classes (Hash(..), hash)


-- Constants
fnv1aBits = 32
fnv1aBlockSize = 8
fnv1a32Offset = 2166136261
fnv1a32Prime  = 16777619

-- Data declarations

-- |Digest in progress
data FNV1aPartial = FNV1aPar Word32 deriving(Eq, Ord)

-- |Hash state
data FNV1aContext = FNV1aCtx { dig :: FNV1aPartial,
			       prime :: Word32,
                               len :: Word64 }

-- |Completed hash
data FNV1aDigest = FNV1aDigest FNV1aPartial deriving(Eq, Ord)

-- instances

instance Show FNV1aDigest where
	show (FNV1aDigest (FNV1aPar n)) = show n

instance Binary FNV1aDigest where
    get = do s <- get
             return $ FNV1aDigest s
    put (FNV1aDigest s) = put s

instance Binary FNV1aPartial where
    put (FNV1aPar w) = put w
    get = do w <- get
  	     return $ FNV1aPar w

instance Binary FNV1aContext where
    put (FNV1aCtx d p l) = put d >> put p >> put l
    get = do d <- get
             p <- get
	     l <- get
	     return $ FNV1aCtx d p l

instance S.Serialize FNV1aDigest where
    put (FNV1aDigest p) = S.put p
    get = do p <- S.get
             return $ FNV1aDigest p

instance S.Serialize FNV1aContext where
    put (FNV1aCtx d p l) = S.put d >> S.put d >> S.put l
    get = do d <- S.get
             p <- S.get
             l <- S.get
             return $ FNV1aCtx d p l

instance S.Serialize FNV1aPartial where
    put (FNV1aPar w) = S.put w
    get = do w <- S.get
             return $ FNV1aPar w

instance Hash FNV1aContext FNV1aDigest where
    outputLength = Tagged 32
    blockLength  = Tagged 8
    initialCtx   = fnv1aInitialContext
    updateCtx    = fnv1aUpdate
    finalize     = fnv1aFinalize


-- API functions

-- |FNV-1a hashing function - this is probably what you want
fnv1a :: L.ByteString -> FNV1aDigest

-- |Initial hash state (crypto-api)
fnv1aInitialContext :: FNV1aContext

-- |Add more data to an existing hash (crypto-api)
fnv1aUpdate :: FNV1aContext -> B.ByteString -> FNV1aContext

-- |Complete a hash (crypto-api)
fnv1aFinalize :: FNV1aContext -> B.ByteString -> FNV1aDigest

-- API function implementations
fnv1a = hash

fnv1aInitialContext = FNV1aCtx (FNV1aPar fnv1a32Offset) fnv1a32Prime 0

crunch :: FNV1aContext -> Word8 -> FNV1aContext
crunch (FNV1aCtx (FNV1aPar h) p l) n = FNV1aCtx (FNV1aPar ((h `xor` fromIntegral n) * p)) p (l + 1)

fnv1aUpdate = B.foldl' crunch

fnv1aFinalize ctx bs = FNV1aDigest $ dig (fnv1aUpdate ctx bs)

