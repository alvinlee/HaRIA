module RTMP.Protocol.RTMP
  ( RTMPData (..)
  ) where

import qualified Data.ByteString.Lazy as BS
import Control.Monad (liftM3,when)

import BinaryPlus
import RTMP.Protocol.HandShake
import RTMP.Protocol.Message

--------------------------------------------------------------------------------
data RTMPData = RTMPData { fstHndShkVal :: HandShake
                         , sndHndShkVal :: HandShake
                         , msgSeqVal    :: Messages
                         } deriving(Show,Eq)
instance Binary RTMPData where
  get = do
    flag <- get :: Get Word8
    when (flag /= 0x03) $ fail "Hand shake flag error!"
    liftM3 RTMPData get get get

  put (RTMPData fstHndShk sndHndShk msgSeq) =
    put (0x03 :: Word8) >> put fstHndShk >> put sndHndShk >> put msgSeq

