module RTMP.Protocol.HandShake
  ( HandShake (..)
  ) where

import qualified Data.ByteString.Lazy as BS
import Control.Monad (liftM)
import BinaryPlus

--------------------------------------------------------------------------------
newtype HandShake = HandShake { hsVal :: BS.ByteString } deriving(Show,Eq)
instance Binary HandShake where
  get = liftM HandShake $ getLazyByteString 1536
  put (HandShake val) = putLazyByteString $ BS.take 1536 val
