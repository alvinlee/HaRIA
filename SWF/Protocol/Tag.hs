module SWF.Protocol.Tag where

import Data.Word

data Tag = Tg { tCode   :: Word16 -- 10bits
              , tLength :: Word32 -- 6bit+4bytes(6bits is 0x3f)
              } deriving (Show,Eq)