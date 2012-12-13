module RTMP.Protocol.Ping
  ( PingType (..)
  , PingData (..)
  ) where

import qualified Data.ByteString.Lazy as BS

import BinaryPlus
import Control.Monad (liftM2)

--------------------------------------------------------------------------------
data PingType = PingClear
              | PingPlay
              | PingBuffer
              | PingReset
              | PingRequest
              | PingResponse
              | PingUnknown Word16
                deriving(Show,Eq)

instance Binary PingType where
  get = do
    code <- get :: Get Word16
    return $ case code of
      0x00 -> PingClear
      0x01 -> PingPlay
      -- 0x02 ->       
      0x03 -> PingBuffer
      0x04 -> PingReset
      -- 0x05 -> 
      0x06 -> PingRequest
      0x07 -> PingResponse      
      -- 0x08 ->       
      _    -> PingUnknown code
      
  put pingType = put $
    case pingType of
      PingClear        -> 0x00
      PingPlay         -> 0x01
      --               -> 0x02
      PingBuffer       -> 0x03 
      PingReset        -> 0x04
      --               -> 0x05
      PingRequest      -> 0x06
      PingResponse     -> 0x07
      --               -> 0x08
      PingUnknown code -> code
    
--------------------------------------------------------------------------------
data PingData = PingData PingType [Word32] deriving(Show,Eq)
instance Binary PingData where
  get = liftM2 PingData get $ safeGetList get
  put (PingData typ parm) = put typ >> mapM_ put parm

