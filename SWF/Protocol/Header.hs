module SWF.Protocol.Header where

import Data.Word

data Header = Hdr { hSig1    :: Word8 -- F/C
                  , hSig2    :: Word8 -- W
                  , hSig3    :: Word8 -- S
                  , hVersion :: Word8
                  , hLength  :: Word32
                  , hSize    :: RECT
                  , hRate    :: FixNum
                  , hCount   :: Word16
                  } deriving (Show,Eq)