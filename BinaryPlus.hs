module BinaryPlus
  ( Word24 (..)
  , Double64 (..)
  , safeGetList
  , module Data.Binary.Get
  , module Data.Binary.Put
  , module Data.Binary
  , module Data.Bits         
  ) where

import Control.Monad (liftM,liftM2)
import Data.Binary hiding (Get,Put,getWord8,putWord8)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import Foreign

--------------------------------------------------------------------------------
newtype Word24 = Word24 { w24Val :: Word32 } deriving(Eq,Show)
instance Binary Word24 where
  get = liftM Word24 getWord24be
  put = putWord24be . w24Val
  
getWord24be :: Get Word32
getWord24be = do
  s <- getBytes 3
  return $!(fromIntegral (s `B.index` 0) `shiftL` 16) .|.
           (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
           (fromIntegral (s `B.index` 2) )

putWord24be :: Word32 -> Put
putWord24be val = do
  putWord8 (fromIntegral (shiftR val 16) :: Word8)
  putWord16be (fromIntegral (val) :: Word16)

--------------------------------------------------------------------------------
newtype Double64 = Double64 { d64Val :: Double } deriving(Eq,Show)
instance Binary Double64 where
  get = liftM Double64 getDouble64be
  put = putDouble64be . d64Val

getDouble64be :: Get Double
getDouble64be = do
  bs <- getBytes 8
  let (fp,offset,sz) = B.toForeignPtr $ B.reverse bs
  if (offset /= 0) || (sz /= 8) then return $ error "getDouble64be need more bytes!"
     else return . B.inlinePerformIO $ withForeignPtr fp (peek . castPtr)

double2word :: Double -> IO Word64
double2word db = 
  with db $ \dbptr -> peek (castPtr dbptr :: Ptr Word64)

putDouble64be :: Double -> Put
putDouble64be = putWord64be . B.inlinePerformIO . double2word

--------------------------------------------------------------------------------
safeGetList :: Get a -> Get [a]
safeGetList geta = do
  end <- isEmpty
  if end then return []
    else liftM2 (:) geta $ safeGetList geta

-- getWord8Safe :: Get Word8
-- getWord8Safe = getWord8 -- do
  -- s <- getBytes 1
  -- return $! (fromIntegral (s `B.index` 0))

--------------------------------------------------------------------------------
-- safe or not safe get              
-- test:: IO ()
-- test = do
--   print $ runGet testBS BS.empty
--   return ()

-- -- testBS :: Get BS.ByteString
-- testBS = do
-- -- lookAhead $ getBytes 1
--   getWord8Safe
-- -- getLazyByteString 1536
