module RTMP.Protocol.Object.Application
  ( Application (..)
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as DM

import RTMP.Protocol.Object.Client
import Network.Socket (SockAddr)

type Clients = DM.Map SockAddr Client

data Application = Application { appNameVal :: BS.ByteString
                               , clientsVal :: Clients
                               } deriving(Eq,Show)
